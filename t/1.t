use strict;
use warnings;
use Test::More tests => 34 ;

BEGIN {use_ok("Hash::Type");}

# create a new hash type
my $personType = new Hash::Type(qw(firstname lastname city));

isa_ok($personType, 'Hash::Type');

# there is more than one way to create tied hashes 

# 1) use the tie syntax, with initial values
my %wolfgang;
tie %wolfgang, $personType, "wolfgang amadeus", "mozart", "salzburg";

isa_ok(tied(%wolfgang), 'Hash::Type');
isa_ok(\%wolfgang, 'HASH');

is($wolfgang{firstname}, "wolfgang amadeus", "wolfgang firstname");
is($wolfgang{lastname}, "mozart", "wolfgang lastname");


# 2) use object-oriented syntax
my $ludwig = new $personType ("ludwig", "van beethoven", "vienna");

isa_ok($ludwig, 'HASH');

is($ludwig->{city}, "vienna", "ludwig city");

is(tied(%wolfgang)->[2], "mozart", "tied wolfgang 2");
is(tied(%$ludwig)->[1], "ludwig", "tied ludwig 1");


# 3) create an empty tied hash and fill the values later
my $jsb = new $personType;
$jsb->{city} = "leipzig";
@{$jsb}{qw(firstname lastname)} = ("johann sebastian", "bach");

isa_ok($jsb, 'HASH');
ok(eq_hash($jsb, {firstname => "johann sebastian",
		  lastname => "bach",
		  city => "leipzig"}), "jsb values");

# dynamically add field names to a hash type; applies to all tied hashes
is($personType->add("lastname", "birth", "death", "birth"), 2, "2 new names");

ok(eq_set([keys %$personType], 
	  [qw(firstname lastname city birth death)]), "proper set of names");


#test the 'names' method

ok(eq_array([$personType->names],
	    [qw(firstname lastname city birth death)]), "proper method 'names'");




$wolfgang{birth} = 1750;
is($personType->{birth}, 4, "added field");
is($wolfgang{birth}, 1750, "wolfgang birth");

$ludwig->{birth} =  1770;
$jsb->{birth} = 1685;

# does this type have this field name ?
ok(not (exists $ludwig->{foobar}), "not exists foobar (instance)");
ok(not (exists $wolfgang{foobar}), "not exists foobar (instance2)");
ok(not (exists $personType->{foobar}), "not exists foobar (type)");
ok(exists $ludwig->{city}, "ludwig exists city");
ok(exists $wolfgang{city}, "wolfgang exists city");


# get back the Hash::Type object from which a tied hash was created
is ($ludwig->{'Hash::Type'}, $personType, "get back hash::type");

# but this is a readonly key
eval {$ludwig->{'Hash::Type'} = "foobar";};
ok($@, "setting ->{'Hash::Type'} is forbidden : $@");


# B) replace Time::gmtime and Time::localtime
my $timeType = new Hash::Type qw(sec min hour mday mon year wday yday);
my $localtime = new $timeType (localtime);
my $gmtime = new $timeType (gmtime);
my $diff = $localtime->{hour} - $gmtime->{hour};
ok("$diff hours difference to GMT");


# lines below break (intentionally)
eval {$jsb->{sons} = "johann-christian et al.";};
ok($@, "die on wrong fields : $@");


eval {$jsb->add('sons');};
ok($@, "die on calling 'add' on a tied hash: $@");




# comparison functions

no warnings 'uninitialized';

my @people = (\%wolfgang, $ludwig, $jsb,
	      new $personType (qw(claudio monteverdi mantova 1567)));

my $byAge = $personType->cmp("birth : -num, lastname, firstname");

isa_ok($byAge, 'CODE', "byAge");

my @p1 = sort $byAge @people;

$, = "\t";
$\ = "\n";

print "By age";
print values %$_ foreach @p1;

my $byNameLength = $personType->cmp(lastname => sub {length($b) <=> length($a)},
	                            lastname => 'alpha', 
                                    firstname => 'alpha');


print "By NameLength";
print values %$_ foreach sort $byNameLength @people;



# test the CLEAR function
%wolfgang = ();
is(keys %wolfgang, 5, 'values cleared, keys still there');
is($wolfgang{lastname}, undef, 'wolfgang array is cleared');


# test the DELETE function

eval {delete $jsb->{city};};
ok($@, "delete forbidden on tied hash : $@");

ok(delete $personType->{city}, "delete OK on $personType");

eval {my $foo = $jsb->{city}; };
ok($@, "city field was really deleted  : $@");

delete tied(%$jsb)->[$personType->{firstname}];
is($jsb->{firstname}, undef, "jsb lost his name");