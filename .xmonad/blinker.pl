#!/usr/bin/perl

$| = 1;

$blink = 1;

while (<STDIN>) {

    if ($_ =~ m/\^blink\(1\).+\^blink\(0\)/) {


        if ($blink) {
            $_ =~ s/\^blink\(1\)\s*\^fg\(([^)]+)\)\s*\^bg\(([^)]+)\)/^fg($2)^bg($1)/;
        }

        $_ =~ s/\^blink\(\d+\)//g;
        
        $blink = ! $blink;
    }

    print $_;
}
