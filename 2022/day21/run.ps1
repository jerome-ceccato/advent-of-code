$inputData = ((Get-Content -Raw -Path 'input') -replace ': ', ' = ') -replace '\n',"`n    "
$template = (Get-Content -Raw -Path 'main.hs') -replace '-- {{part1}}', $inputData
$part2Input = ($inputData -replace "    root = rvrh \+ hzgl`n",'') -replace "    humn = 2116`n",''
$template = $template -replace '-- {{part2}}', $part2Input
$template | Set-Content -Path 'generated.hs'
runghc .\generated.hs
