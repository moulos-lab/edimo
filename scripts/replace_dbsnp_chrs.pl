#!/usr/bin/perl

use strict;
use File::Basename;
use File::Spec;
use Getopt::Long;
use IO::Uncompress::Gunzip qw(gunzip $GunzipError);
use Pod::Usage;

#use Data::Dumper;

select(STDOUT);
$|=1;

# Cleanup code
$SIG{INT} = \&catch_cleanup;

# Set defaults
our $scriptname = "replace_dbsnp_chrs.pl";
our $scriptversion = "0.01";
our $mapfile;
our $dbsnp;
our $dout;
our $style = "ensembl";
our $canonical;
our $silent;
our $man;
our $ver;
our $help;

# Validate main arguments
&check_inputs;

# https://github.com/dpryan79/ChromosomeMappings/blob/master/GRCh37_NCBI2UCSC.txt
my %map;
open(MAP,$mapfile) or die "\nCan't open $mapfile\n";
print "Reading map file\n";
while (my $line = <MAP>) {
    $line =~ s/\r|\n$//g;
    my @conts = split("\t",$line);
    $map{$conts[0]} = $conts[1];
}
close(MAP);

my $notfound = 0;
my $excluded = 0;

open(OUT,">$dout");
if ($dbsnp =~ m/\.gz$/) {
    my $z = new IO::Uncompress::Gunzip $dbsnp 
        or die "\ngunzip failed: $GunzipError\n";
    while (my $line = <$z>) {
        $line =~ s/\r|\n$//g;
        
        print "Processed $. lines\n" if ($.%1000000 == 0 && !$silent);
        
        if ($line =~ m/^#/i) {
            print OUT $line,"\n" ;
        }
        else {
            my @dbline = split("\t",$line);
            $dbline[0] = $map{$dbline[0]};
            if (!$dbline[0]) {
                $notfound++;
                next;
            }
            if ($dbline[0] =~ m/chrUn|random|fix|alt|NW|GL|hap|cox/ 
                && !$canonical) {
                $excluded++;
                next;
            }
            $dbline[0] =~ s/chrM/chrMT/g if $dbline[0] =~ m/chrM/;
            $dbline[0] =~ s/chr//g if ($style eq "ensembl");
            print OUT join("\t",@dbline),"\n";
        }
    }
}
else {
    open(DB,$dbsnp) or die "\nCan't open $dbsnp\n";
    while (my $line = <DB>) {
        $line =~ s/\r|\n$//g;
        
        print "Processed $. lines\n" if ($.%1000000 == 0 && !$silent);
        
        if ($line =~ m/^#/i) {
            print OUT $line,"\n" ;
        }
        else {
            my @dbline = split("\t",$line);
            $dbline[0] = $map{$dbline[0]};
            if (!$dbline[0]) {
                $notfound++;
                next;
            }
            if ($dbline[0] =~ m/chrUn|random|fix|alt|NW|GL|hap|cox/ 
                && !$canonical) {
                $excluded++;
                next;
            }
            $dbline[0] =~ s/chrM/chrMT/g if $dbline[0] =~ m/chrM/;
            $dbline[0] =~ s/chr//g if ($style eq "ensembl");
            print OUT join("\t",@dbline),"\n";
        }
    }
}
close(OUT);

print "$notfound lines excluded because of no mapping\n" if ($notfound > 0);
print "$excluded lines excluded because of canonical\n" if ($excluded > 0);

sub check_inputs {
    my $stop;
    GetOptions(
        "map|a=s" => \$mapfile,
        "dbsnp|d=s" => \$dbsnp,
        "output|o=s" => \$dout,
        "style|t=s" => \$style,
        "noncanonical|c" => \$canonical,
        "man|m" => \$man,
        "silent|s" => \$silent,
        "version|v" => \$ver,
        "help|h" => \$help
    );
    
    Pod::Usage::pod2usage( -verbose => 1, -exitstatus => 0 ) if ($help);
    Pod::Usage::pod2usage( -exitstatus => 0, -verbose => 2 ) if ($man);
    
    if ($ver) {
        print "\n\n$scriptname version $scriptversion\n\n";
        exit;
    }
    
    $stop .= "--- Please specify RefSeq to chromosome map file ---\n" 
        if (!$mapfile);
    $stop .= "--- Please specify input dbSNP file ---\n" 
        if (!$dbsnp);
    $stop .= "--- Please specify output dbSNP file ---\n" 
        if (!$dout);
    $stop .= "--style must be one of ensembl, ucsc\n"
        if ($style && $style !~ m/^ensembl|ucsc$/);
    if ($stop) {
        print "\n$stop\n";
        print "Type perl $scriptname --help for help in usage.\n\n";
        exit;
    }
}

sub catch_cleanup {
    print STDERR "\nCatching Ctrl-C, cleaning temporary files!\n";
    &cleanup;
    die;
}

sub cleanup {
    #remove_tree($tmpdir);
}

__END__

=pod

=head1 NAME

replace_dbsnp_chrs.pl - Replace chromosome names in latest dbSNP versions

=head1 SYNOPSIS

replace_dbsnp_chrs.pl --map <chr_mapfile> --dbsnp <dbSNP in> --output <dbSNP out> [OPTIONS]

Examples:

=over 4

=item

perl replace_dbsnp_chrs.pl --map refseq2ucsc_chrs.txt --dbsnp dbSNP.vcf 
  --output dbSNP_new.vcf --style "ucsc"

=back

=head1 DESCRIPTION

This script reads a mapping file with two columns, RefSeq chromosome names and
UCSC chromosome names, and replaces the chromosome notation in later dbSNP 
versions given the dbSNP VCF file.

=head1 ARGUMENTS

=item mapfile B<(required)>

--map or -a

A text file with two columns and no header. The first column has chromosome 
names in RefSeq notation and the second in UCSC notation. A map file is provided
with this script.

=item dbSNP file B<(required)>

--dbsnp or -d

The input dbSNP VCF file with RefSeq chromosome names.

=item output file B<(required)>

--output or -o

The new dbSNP file to write

=item chromosome name style B<(optional)>

--style or -t

How should the chromosome names be written. It should be ensembl (default) for
1, 2, ..., MT or ucsc for chr1, chr2, ..., chrM. It defaults to ensembl.

=item canonical B<(optional)>

--noncanonical or -c

Should chromosomes and alt sequences or non-anchored contigs be returned or only
canonical chromosomes? (1-22, X, Y, MT). By default, switched on.

=item version B<optional>

--version or -v

Print current version of script.

=item silent B<optional>

--silent or -s

Do not display verbose messages.

=item help B<(optional)>

--help or -h

Display this help text.

=item man B<(optional)>

--man or -m

Display the full manual of the script.

=back

=head1 AUTHOR

Panagiotis Moulos (L<moulos@fleming.gr)>

=cut
