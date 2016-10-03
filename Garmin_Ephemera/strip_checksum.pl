#!/usr/bin/perl

$buf = "";
read(STDIN,$buf,3);
$buf = "";
while(read(STDIN,$buf,2304)){
print $buf;
$buf = "";
read(STDIN,$buf,3);
$buf = "";
}
print $buf;
