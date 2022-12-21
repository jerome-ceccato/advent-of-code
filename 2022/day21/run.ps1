$inputData = (Get-Content -Raw -Path 'input') -replace ': ', ' = '
$template = (Get-Content -Raw -Path 'main.hs') -replace '-- {{part1}}', $inputData
$template | Set-Content -Path 'generated.hs'
runghc .\generated.hs
