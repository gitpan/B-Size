use 5.005;
use ExtUtils::MakeMaker;

open XS, ">b_sizeof.c" or die $!;
if ($] < 5.005_58) {
    print XS <<EOF;
#define PL_op_name op_name
#define PL_op_desc op_desc
typedef double NV;
EOF

}
print XS <<EOF;
static void boot_B_Sizeof(void)
{
    HV *stash = gv_stashpvn("B::Sizeof", 9, TRUE);
EOF

while (<DATA>) {
    chomp;
    next unless $_;
    print XS newCONSTSUB($_);
}
print XS newCONSTSUB("CONDOP") if $] < 5.005_58;
print XS "\n}\n";

WriteMakefile(
    'NAME' => 'B::Size',
    'VERSION_FROM' => 'Size.pm',
    'macro' => {
	  CVSROOT => 'modperl.com:/local/cvs_repository',
    },
);

sub newCONSTSUB {
    my $name = shift;
    qq{   (void)newCONSTSUB(stash, "$name", newSViv(sizeof($name)));\n};
}

sub MY::postamble { 
    return <<'EOF'; 

cvs_tag :
	cvs -d $(CVSROOT) tag v$(VERSION_SYM) . 
	@echo update Size.pm VERSION now 
EOF
} 

__END__
OP
PMOP
UNOP
BINOP
LISTOP
LOGOP
SVOP
GVOP
PVOP
COP
LOOP
SV
HV
AV
NV
IV
CV
GV
GP
U32
U16
U8
XRV
XPV
XPVIV
XPVUV
XPVNV
XPVMG
XPVLV
XPVGV
XPVBM
XPVFM
XPVIO
XPVCV
XPVAV
XPVHV
HE
HEK
MAGIC
REGEXP
