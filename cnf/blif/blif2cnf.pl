#!/usr/bin/perl

# Script for converting blif files into cnf files, by jpms@inesc.pt
#
# Start with the standard front-end. Define switches, and print help
# message is needed.
#
# jpms.

#######require "getopts.pl";
#######&Getopts('hdDm:');		      # -h -d -s, set opt_h, opt_d, etc

use Getopt::Std;
getopts('hdDm:');		      # -h -d -s, set opt_h, opt_d, etc

if ($opt_h) {
    print "script: generates CNF formula for a circuit in BLIF format.\n";
    print "author: jpms\@inesc.pt\n\n";
    print "Usage(s): blif2cnf[.pl] [-h] [-d] [-D] [-m fname] input-files\n";
    print "          blif2cnf[.pl] [-h] [-d] [-D] [-m fname] < input-file\n";
    print "\t -h         This help message\n";
    print "\t -d         Run simple debug mode\n";
    print "\t -D         Run extensive debug mode\n";
    print "\t -m fname   Output map between blif and cnf variables\n";
    exit;
}
if ($opt_D) { $opt_d = 1; }

$new_gate = 0;
$fix_value = 0;

# Main loop. Read lines and act accordingly.
#
while (<>) {
    if ($ARGV ne $oldargv) {
	if ($ARGV eq "-") {
	    select(STDOUT);
	}
	else {
	    $ARGV =~ m/([\w\d\-]+)\.\S/;
	    $outf = $1 . ".cnf";
	    open(FOUT, ">$outf");
	    select(FOUT);
	}
	$oldargv = $ARGV;

	# Prepare new file.
	print "c CNF file generated from BLIF2CNF script.\n";
	print "c Original BLIF file was $oldargv\n";

	undef (%var_map);  # Hash table of vars.
	$var_idx = 1;      # Variable IDs.
	undef (@clauses);
	$cl_idx = 0;
	$cl_count = 0;
    }
    if ($opt_d) { print STDERR "Read $_"; }
    if (m/^\#/ || m/^\s*$/ || m/\.default/) {
	next;
    }
    elsif (m/\.names\s+([\w\d\-\_\.\(\)\[\]\s]+)/) {
#    print "inside else \n";
	$names = $1;
#	print "names = $names \n";
	if (m/\\/) {    # Contemplate a *single* line extension.
#	print "after match = $_ \n";

	    do {
#	    	print "$_ \n";

		$line = <>;
		$line =~ m/([\w\d\-\_\.\(\)\s]+)/;
		$names .= " $1";
		
	#	print "line = $line \n";

	    }
	    while ($line =~ m/\\/);
	    
	}
	# Add final clause relating extra variables (@var_stack) with output.
	if ($new_gate) {
	    if ($fix_value) {
		$clauses[$cl_idx++] = "-$var_map{$gt_names[$#gt_names]} 0\n";
		$fix_value = 0;
	    }
	    else {
		$ncl = "";
		$ncl .= "$o_sgnN$var_map{$gt_names[$#gt_names]} ";
		for ($idx = 0; $idx < $st_idx; $idx++) {
		    $ncl .= "$var_stack[$idx] ";
		}
		$ncl .= " 0\n";
		$clauses[$cl_idx++] = $ncl;
	    }
	    $cl_count++;
	    undef @gt_names;
	}
	@gt_names = split /\s+/, $names;
	if ($opt_D) { &prt_array ("GT NAMES: ", \@gt_names); }

	if ($#gt_names == 0) {  # Fixed value defined.
	    $fix_value = 1;
	}
	# Assign numbers to variables.
	undef(@varID);
	$pos_idx = 0;
	foreach $gt_name (@gt_names) {
	    if ($gt_name ne "") {
		if (!defined($var_map{$gt_name})) {
		    $var_map{$gt_name} = $var_idx++;
		}
		$varID[$pos_idx++] = $var_map{$gt_name};
	    }
	    else { if ($opt_d) { print STDERR "Empty gate name??\n"; } }
	}
	undef (@var_stack);
	$st_idx = 0;
	$o_sgnP = "NONE"; $o_sgnN = "NONE";
	$new_gate = 1;    # Declare a new gate.
    }
    elsif (m/\.end\s+/) {
	if ($new_gate) {	
	    if ($fix_value) {
		$clauses[$cl_idx++] = "-$var_map{$gt_names[$#gt_names]} 0\n";
		$fix_value = 0;
	    }
	    else {
		$ncl = "";
		$ncl .= "$o_sgnN$var_map{$gt_names[$#gt_names]} ";
		for ($idx = 0; $idx < $st_idx; $idx++) {
		    $ncl .= "$var_stack[$idx] ";
		}
		$ncl .= " 0\n";
		$clauses[$cl_idx++] = $ncl;
	    }
	    $cl_count++;
	    undef @gt_names;
	}
	&prt_clauses;
	if (defined($opt_m)) {
	    &prt_map ($opt_m, \%var_map);
	}
	###&prt_clause_list ($var_idx, \@clauses);
	if ($opt_d) {
	    print STDERR "NUMBER OF CLAUSES: $cl_count\n";
	    print STDERR "End of file reached.\n";
	    &prt_var_map (\%var_map);
	}
    }
    elsif (m/\.model/ || m/\.inputs/ || m/\.outputs/) {
	if ($opt_d) { print STDERR "Declaration found: $_" }
	if (m/\\/) {    # Contemplate line extensions.
	    do {
		$line = <>;
	    }
	    while ($line =~ m/\\/);
	}
    }
    elsif (m/([01\-]+)\s+([01])/) {  # It's either 0 or 1 for all rows!!
	@gvalues = split / */, $1;
	if ($opt_D) { &prt_array ("GT VALUES: ", \@gvalues); }
	$goutv = $2;
	if ($opt_d) { print STDERR "GOUTV: $goutv\n"; }
	if ($goutv) { $o_sgnP = ""; $o_sgnN = "-"; }
	else        { $o_sgnP = "-"; $o_sgnN = ""; }

	$cl_count += $#gvalues + 1 + 1;  # for this row.
	if ($opt_d && $#gt_names != $#gvalues+1_) {
	    print STDERR "ERROR: Invalid cube length\n";
	}
	# Create a new variable. Record variable ID in stack.
	# Create clauses between cube and new variable, and between
	# new variable and output.

	# 1. Create variable.
	$var_name = "zxyxz" . $var_idx;
	$var_map{$var_name} = $var_idx++;

	# 2. Record ID.
	$xvID = $var_idx - 1;           # ID of added extra variable.
	$var_stack[$st_idx++] = $xvID;  # Add new variable to stack.

	# 3.a Create clauses between cube and new variable.
	$idx = 0;
	foreach $gval (@gvalues) {
	    if ($gval eq "0") {
		$clauses[$cl_idx++] = "-$varID[$idx] -$xvID 0\n";
	    }
	    elsif ($gval eq "1") {
		$clauses[$cl_idx++] = "$varID[$idx] -$xvID 0\n";
	    }
	    else { $cl_count--; }
	    $idx++;
	}
	$ncl = "$xvID ";
	$idx = 0;
	foreach $gval (@gvalues) {
	    if ($gval eq "0") {
		$ncl .= "$varID[$idx] ";
	    }
	    elsif ($gval eq "1") {
		$ncl .= "-$varID[$idx] ";
	    }
	    $idx++;
	}
	$ncl .= " 0\n";
	$clauses[$cl_idx++] = $ncl;
	# 3.b Create clauses between new variable and output.
	$clauses[$cl_idx++] =
	    "-$xvID $o_sgnP$var_map{$gt_names[$#gt_names]} 0\n";
	$cl_count++;
    }
    elsif (m/^\s*([10])\s*$/) {
	if ($fix_value) {
	    if ($1 == 1) { $sgn = ""; }
	    else         { $sgn = "-"; }
	    $clauses[$cl_idx++] = "$sgn$var_map{$gt_names[$#gt_names]} 0\n";
	    $cl_count++;
	    $fix_value = 0;
	    $new_gate = 0;
	}
	else { print STDERR "Invalid term specification: $_\n"; }
    }
    else {
	print STDERR "Invalid line in BLIF file: $_Check syntax.\n";
    }
    if ($opt_d) { print STDERR "CLAUSES: $cl_count\n"; }
}

1;

sub prt_clause_list {
    my $vnum = shift @_;
    my $cl_list = shift @_;
    $vnum--;
    my $clnum = $#$cl_list + 1;
    print "p cnf $vnum $clnum\n";
    foreach (my $idx = 0; $idx <= $#$cl_list; $idx++) {
	print "$$cl_list[$idx]";
    }
}

sub prt_clauses {
    my $vnum = $var_idx - 1;
    my $clnum = $#clauses + 1;
    print "p cnf $vnum $clnum\n";
    foreach (my $idx = 0; $idx <= $#clauses; $idx++) {
	print "$clauses[$idx]";
    }
}

sub prt_array {
    local ($str, $vect_ref) = @_;

    print STDERR "$str";
    foreach $v (@$vect_ref) {
	print STDERR "$v ";
    }
    print STDERR "\n";
}

sub prt_var_map {
    local $vm_ref = shift @_;
    print STDERR "Variable Mappings:\n";
    if ($opt_D) {
	@vars = sort keys %$vm_ref;
	print STDERR "VARS: @vars\n";
    }
    foreach $var (sort keys %$vm_ref) {
	print STDERR "$var -> $$vm_ref{$var}\n";
    }
}

sub prt_map {
    local ($fname, $vm_ref) = @_;
    #print "Variable Mappings:\n";
    if ($opt_D) {
	@vars = sort keys %$vm_ref;
	print STDERR "VARS: @vars\n";
    }
    local *OUTP;
    open (OUTP, ">$opt_m") || die "Unable to open map file $opt_m";
    foreach $var (keys %$vm_ref) {
	print OUTP "$var    $$vm_ref{$var}\n";
    }
    close INP;
}


