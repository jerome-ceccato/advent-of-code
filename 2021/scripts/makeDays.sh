#!/bin/zsh

template=8
templateFormatted="08"
for day in {9..25}
do
    formatted=$(printf "%02d" day)
    oldDay="day$templateFormatted"
    newDay="day$formatted"
    
    rm -rf $newDay
    cp -vr $oldDay $newDay
    mv -v "$newDay/$oldDay.swift" "$newDay/$newDay.swift"
    mv -v "$newDay/$oldDay.txt" "$newDay/$newDay.txt"
    
    sed -i '' "s/$oldDay/$newDay/g" "$newDay/$newDay.swift"
    sed -i '' "s/Day$templateFormatted/Day$formatted/g" "$newDay/$newDay.swift"
    
    sed -i '' "s/Day $template/Day $day/g" "$newDay/README.md"
    sed -i '' "s/day\/$template/day\/$day/g" "$newDay/README.md"

    echo "Created day $day"
done
