# B::TerseSize.pm
# Copyright (c) 1999 Doug MacEachern. All rights reserved.
# This module is free software; you can redistribute and/or modify
# it under the same terms as Perl itself.

# portions of this module are based on B::Terse, by Malcolm Beattie

package B::TerseSize;

use strict;
use B ();
use B::Asmdata qw(@specialsv_name);
use B::Size ();

{
    no strict;
    $VERSION = '0.03';
}

my $opcount;
my $opsize;
my $copsize;
my $curcop;

sub UNIVERSAL::op_size {
    $opcount++;
    my $size = shift->size;
    $opsize += $size;
    $copsize += $size;
}

sub peekop {
    my $op = shift;

    my $size = $op->size;
    $opcount++;
    $opsize += $size;
    $copsize += $size;
    (my $ppaddr = $op->ppaddr) =~ s/^Perl_pp_//;
    return sprintf("%-6s %-12s {%d bytes}", 
		   B::class($op), $ppaddr, $size);
}

my $hr = "=" x 60;
my %filelex = ();

sub package_size {
    my($package) = @_;

    #local *UNIVERSAL::op_size = \&universal_op_size;

    my %retval = ();
    my $total_opsize = 0;
    my $total_opcount = 0;
    my $stash;
    {
	no strict;
	$stash = \%{"$package\::"};
    }

    for (keys %$stash) {
	my $name = $package . "::$_";
	my $has_code = 0;

	{
	    no strict;
	    $has_code = *{$name}{CODE}; #defined() expects CvROOT || CvXSUB
	}

	unless ($has_code) { #CV_walk will measure
	    $total_opsize += 
	      B::Sizeof::GV + B::Sizeof::XPVGV + B::Sizeof::GP;
	}

	#measure global variables
	for my $type (qw(ARRAY HASH SCALAR)) {
	    no strict;
	    next if $name =~ /::$/; #stash
	    next unless /^[\w_]/;
	    next if /^_</;
	    my $ref = *{$name}{$type};
	    next unless $ref;
	    my $obj = B::svref_2object($ref);
	    next if ref($obj) eq 'B::NULL';
	    my $tsize = $obj->size;
	    $total_opsize += $tsize;
	    $retval{"*${_}{$type}"} = {'size' => $tsize};
	}

	next unless defined $has_code;

	CV_walk('slow', $name, 'op_size');

	for (keys %{ $filelex{$package} }) {
	    my $fsize = $filelex{$package}->{$_};
	    $total_opsize += $opsize;
	    $retval{"my ${_} = ...;"} = 
	      {'size' => $fsize};
	}
	%filelex = ();
	$total_opsize  += $opsize;
	$total_opcount += $opcount;
	$retval{$_} = {'count' => $opcount, 'size' => $opsize};
    }

    return (\%retval, $total_opcount, $total_opsize);
}

sub CV_walk {
    my($order, $objname, $meth) = @_;

    $meth ||= 'terse_size';
    my $cvref = \&{$objname};
    my $cv = B::svref_2object($cvref);
    my($package, $func) = ($objname =~ /(.*)::([^:]+)$/);

    $opsize  = B::Sizeof::GV + B::Sizeof::XPVGV + B::Sizeof::GP;
    $opcount = 0;
    $curcop = "";

    my $gv = $cv->GV;
    $opsize += length $gv->NAME;

    if (my $stash = $cv->is_alias($package)) {
	return;
    }

    $opsize += B::Sizeof::XPVCV;

    $opsize += B::Sizeof::SV;
    if ($cv->FLAGS & B::SVf_POK) {
	$opsize += B::Sizeof::XPV + length $cv->PV;
    }
    else {
	$opsize += B::Sizeof::XPVIV; #IVX == -1 for no prototype
    }

    init_curpad_names($cvref);

    if ($order eq 'exec') {
	B::walkoptree_exec($cv->START, $meth);
    } else {
	B::walkoptree_slow($cv->ROOT, $meth);
    }

    curcop_info() if $curcop;

    my($padsize, $padsummary) = PADLIST_size($cv);
    $opsize += $padsize;

    $padsummary;
}

sub terse_size {
    my($order, $objname) = @_;

    my $padsummary = CV_walk($order, $objname);

    print "\n$hr\nTotals: $opsize bytes | $opcount OPs\n$hr\n";

    if ($padsummary) {
	print "\nPADLIST summary:\n";
	print @$padsummary;
    }
}

my @curpad_names = ();

sub init_curpad_names {
    my $cv = B::svref_2object(shift);
    my $padlist = $cv->PADLIST;
    return if ref($padlist) eq 'B::SPECIAL';
    @curpad_names = ($padlist->ARRAY)[0]->ARRAY;
}

sub compile {
    my $order = shift;
    my @options = @_;
    B::clearsym() if defined &B::clearsym;

    if (@options) {
	return sub {
	    my $objname;
	    foreach $objname (@options) {
		$objname = "main::$objname" unless $objname =~ /::/;
		terse_size($order, $objname);
	    }
	}
    } else {
	if ($order eq "exec") {
	    return sub { B::walkoptree_exec(B::main_start, "terse_size") }
	} else {
	    return sub { B::walkoptree_slow(B::main_root, "terse_size") }
	}
    }
}

sub indent {
    my $level = shift;
    return "    " x $level;
}

#thanks B::Deparse
sub padname {
    my $obj = shift;
    return '?' unless ref $obj;
    my $str = $obj->PV;
    my $ix = index($str, "\0");
    $str = substr($str, 0, $ix) if $ix != -1;
    return $str;
}

sub B::OP::terse_size {
    my ($op, $level) = @_;
    my $t = $op->targ;
    my $targ = "";
    if ($t > 0) {
	my $name = B::OP::op_name($op->targ);
	my $desc = B::OP::op_desc($op->targ);
	if ($op->type == 0) { #OP_NULL
	    $targ = $name eq $desc ? " [$name]" : 
	      sprintf " [%s - %s]", $name, $desc;
	}
	else {
	    $targ = sprintf " [targ %d - %s]", $t, 
	    padname($curpad_names[$t]);
	}
    }
    print indent($level), peekop($op), $targ, "\n";
}

sub B::SVOP::terse_size {
    my ($op, $level) = @_;
    print indent($level), peekop($op), "  ";
    $op->sv->terse_size(0);
}

sub B::GVOP::terse_size {
    my ($op, $level) = @_;
    print indent($level), peekop($op), "  ";
    $op->gv->terse_size(0);
}

sub B::PMOP::terse_size {
    my ($op, $level) = @_;
    my $precomp = $op->precomp;
    print indent($level), peekop($op),
    (defined($precomp) ? " /$precomp/\n" : " (regexp not compiled)\n");
}

sub B::PVOP::terse_size {
    my ($op, $level) = @_;
    print indent($level), peekop($op), " ", B::cstring($op->pv), "\n";
}

my $hr2 = "-" x 60;

sub curcop_info {
    my $line = $curcop->line;
    my $linestr = "line $line";

    if ($line > 0 && $ENV{MOD_PERL}) {
	my $anchor = "";
	if ($line > 10) {
	    $anchor = "#" . ($line - 10);
	}
	my $args = sprintf "noh_fileline=1&filename=%s&line=%d", 
	$curcop->filegv->SV->PV, $line;
	my $uri = Apache->request->location;
	$linestr = qq(<a href="$uri?$args$anchor">$linestr</a>);
    }

    print "\n[$linestr size: $copsize bytes]\n";
}

sub B::COP::terse_size {
    my ($op, $level) = @_;

    my $label = $op->label;
    if ($label) {
	$label = " label ".B::cstring($label);
    }

    curcop_info() if $curcop;

    $copsize = 0;
    $curcop = $op;

    print "\n$hr2\n", indent($level), peekop($op), "$label\n";
}


sub B::PV::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    my $pv = B::cstring($sv->PV);
    B::Size::escape_html(\$pv) if $ENV{MOD_PERL};
    printf "%s %s\n", B::class($sv), $pv;
}

sub B::AV::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s FILL %d\n", B::class($sv), $sv->FILL;
}

sub B::GV::terse_size {
    my ($gv, $level) = @_;
    my $stash = $gv->STASH->NAME;
    if ($stash eq "main") {
	$stash = "";
    } else {
	$stash = $stash . "::";
    }
    print indent($level);
    printf "%s *%s%s\n", B::class($gv), $stash, $gv->NAME;
}

sub B::IV::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s %d\n", B::class($sv), $sv->IV;
}

sub B::NV::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s %s\n", B::class($sv), $sv->NV;
}

sub B::RV::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s \n", B::class($sv);
}

sub B::NULL::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s \n", B::class($sv);
}
    
sub B::SPECIAL::terse_size {
    my ($sv, $level) = @_;
    print indent($level);
    printf "%s #%d %s\n", B::class($sv), $$sv, $specialsv_name[$$sv];
}

my $padname_max = 0;

sub PADLIST_size {
    my $cv = shift;
    my $obj = UNIVERSAL::isa($cv, "B::CV") ? $cv : B::svref_2object($cv);

    my $size = (B::Sizeof::AV + B::Sizeof::XPVAV) * 3; #padlist, names, values

    if ($obj->PADLIST->isa('B::SPECIAL')) {
	return B::Sizeof::AV; #XXX???
    }

    my($padnames, $padvals) = $obj->PADLIST->ARRAY;
    my @names = $padnames->ARRAY;
    $padname_max = 0;
    my @names_pv = map {
	my $pv = padname($_);
	$padname_max = length($pv) > $padname_max ? 
	  length($pv) : $padname_max;
	$pv;
    } @names;

    my @vals = $padvals->ARRAY;
    my $fill = $padnames->FILL;
    my $fill_len = length $fill;
    my @retval = ();
    my $wantarray = wantarray;

    for (my $i = 0; $i <= $fill; $i++) {
	my $entsize = $names[$i]->size;
	my $is_fake = $names[$i]->FLAGS & B::SVf_FAKE;
	if ($is_fake) {
	    $entsize += B::Sizeof::SV; # just a reference to outside scope
	    if (B::class($obj->OUTSIDE->GV) eq 'SPECIAL') {
		$filelex{ $obj->GV->STASH->NAME }->{ $names_pv[$i] } = 
		  $vals[$i]->size;
	    }
	    else {
		#XXX nested/anonsubs
	    }
	}
	else {
	    $entsize += $vals[$i]->size;
	}
	$size += $entsize;
	next unless $wantarray;

	my $class = B::class($vals[$i]);
	my $byteinfo = sprintf "[%-4s %3d bytes]",
	$class, $entsize;

	push @retval, sprintf "%${fill_len}d: %${padname_max}s %s %s\n", 
	$i,
	$names_pv[$i], 
	$byteinfo,
	$is_fake ? '__SvFAKE__' : $vals[$i]->sizeval;
    }

    return $wantarray ? ($size, \@retval) : $size;
}

#hmm, I wonder if B::Deparse could be used instead
sub Apache::Status::noh_fileline {
    my $r = shift;
    my %args = $r->args;
    local *FH;
    my $filename = $args{filename};
    $r->send_http_header('text/html');

    unless (Apache::Status::status_config($r, "StatusTerseSize")) {
	print "sorry, StatusTerseSize not enabled\n";
	return 0;
    }

    unless (exists $main::{"_<$filename"}) {
	print "sorry, $filename is not a file used by Perl\n";
	return 0;
    }

    my $i = 1;
    $r->print('<pre>');
    open FH, $filename or die $!;
    while (<FH>) {
	chomp;
	B::Size::escape_html(\$_);
	my $line = ($i == $args{line}) ? 
	  \qq(<font color="#FF0000">$_</font>) : \$_;
	print qq(<a name=$i>$$line\n);
	$i++;
    }
    close FH;

    0;
}

sub max {
    my($cur, $maybe) = @_;
    $maybe > $cur ? $maybe : $cur;
}

my %summary_cache = ();

sub apache_package_size {
    my $package = shift;
    my($subs, $opcount, $opsize);
    my $keys = 0;
    my $cache = {};

    {
	no strict 'refs';
	$keys = keys %{"$package\::"};
    }

    if ($cache = $summary_cache{$package}) {
	if ($cache->{'keys'} == $keys) {
	    return @{ $cache->{'data'} } if $cache->{'data'};
	}
    }

    $cache->{'keys'} = $keys;
    $summary_cache{$package} = $cache;
    @{ $cache->{'data'} } = B::TerseSize::package_size($package);
}

sub status_memory_usage {
    my($r, $q) = @_;

    unless (Apache::Status::status_config($r, "StatusTerseSize")) {
	return ["StatusTerseSize is not enabled"];
    }

    unless ($r->dir_config("StatusTerseSizeMainSummary")) {
	return ["StatusTerseSizeMainSummary is not enabled"];
    }

    my $script = $q->script_name;
    my $stab = Devel::Symdump->rnew('main');
    my %total;
    my @retval = ('<pre>');
    my($clen, $slen, $nlen);

    for my $package ('main', $stab->packages) {
	my($subs, $opcount, $opsize) = apache_package_size($package);
	$total{$package} = {'count' => $opcount, 'size' => $opsize};
	$nlen = max($nlen, length $package);
	$slen = max($slen, length $opsize);
	$clen = max($clen, length $opcount);
    }

    for (sort { $total{$b}->{size} <=> $total{$a}->{size} } keys %total) {
	my $link = qq(<a href="$script/$_?noh_b_package_size">);
	push @retval,
	sprintf "$link%-${nlen}s</a> %${slen}d bytes | %${clen}d OPs\n", 
	$_, $total{$_}->{size}, $total{$_}->{count};
    }
    \@retval;
}

Apache::Status->menu_item(
    'status_memory_usage' => "Memory Usage",
    \&status_memory_usage,
) if $ENV{MOD_PERL} and Apache->module("Apache::Status");

1;

__END__

=head1 NAME

B::TerseSize - Printing info about ops and their (estimated) size

=head1 SYNOPSIS

	perl -MO=TerseSize[,OPTIONS] foo.pl

=head1 DESCRIPTION

The I<B::Size> and I<B::TerseSize> modules attempt to measure the size 
of Perl op codes.  The output of B<B::TerseSize> is similar to that of 
I<B::Terse>, but includes the size of each OP in the tree and the
PADLIST (subroutine lexical variables).  The module can be run just as 
other compiler backends or used via I<Apache::Status> (version 2.02
and higher).

If the I<Apache::Status> I<StatusTerseSize> option is enabled, there
will be a main menu item added, "Memory Usage".  Clicking on this link 
will cause I<B::TerseSize> to produce a summary of package memory
usage.  This summary can take quite a while to produce, as each
package subroutine syntax tree will be walked, adding up the
information.  This information will be cached, so running httpd in
I<-X> (non-forking mode) is a good choice.

When browsing the Apache::Status "Symbol Table Dump", a "Memory
Usage" link will be at the bottom of each page.  These summaries
also include measurements of package global variables.

The Apache::Status symbol table browser will also provide an option to 
dump a subroutine tree along with the other subroutine options.

=head1 CAVEATS

The memory measurements are only an estimate.  But, chances are, if a 
measurement is not accurate, it is smaller than the actual size.

The "execution order" option under Apache::Status can only be run once 
unless you apply the I<patches/b_clearsym_60.pat> to your Perl kit.

=head1 SEE ALSO

B(3), B::Size(3), B::LexInfo(3), Apache::Status(3)

=head1 AUTHOR

Doug MacEachern
based in part on B::Terse by Malcolm Beattie

=cut
