#!/Applications/j903/bin/jconsole

NB. This is the same solution as main.ijs, but explained step by step

filename =. 'input'
readfile =. 1!:1 @: <

NB. split the file by \n. Assumes the input file ends with a \n
cut =. < ;. _2 
splitfile =. cut @: readfile
unboxed =. > splitfile filename

NB. convert into list of int, setting empty lines to -1
asint =. _1 ". unboxed
NB. add a trailing -1 so we can use it to split the array
trailingint =. asint , _1
NB. cut the main list and sum the chunks (calorie count per elf)
cutsum =. +/ ;. _2
flat =. cutsum trailingint

NB. find the max value in the list
part1 =. >. / flat


NB. desc sort
sorted =. \:~ flat
NB. pick the first 3
first =. (i. 3) { sorted
NB. add them
part2 =. +/ first


echo part1
echo part2
exit ''
