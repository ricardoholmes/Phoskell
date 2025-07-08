mkdir -p bench-out

cabal build --enable-benchmarks

echo ''

echo '=================='
echo '| STARTING POINT |'
echo '=================='
cabal bench point --benchmark-options='--output bench-out/point-report.html --json bench-out/point-report.json --csv bench-out/point-report.csv'
echo '-- DONE POINT --'

echo ''

echo '======================'
echo '| STARTING HISTOGRAM |'
echo '======================'
cabal bench histogram --benchmark-options='--output bench-out/histogram-report.html --json bench-out/histogram-report.json --csv bench-out/histogram-report.csv'
echo '-- DONE HISTOGRAM --'

echo ''

echo '========================'
echo '| STARTING CONVOLUTION |'
echo '========================'
cabal bench convolution --benchmark-options='--output bench-out/convolution-report.html --json bench-out/convolution-report.json --csv bench-out/convolution-report.csv'
echo '-- DONE CONVOLUTION --'

echo ''

echo '======================'
echo '| STARTING SYNTHESIS |'
echo '======================'
cabal bench synthesis --benchmark-options='--output bench-out/synthesis-report.html --json bench-out/synthesis-report.json --csv bench-out/synthesis-report.csv'
echo '-- DONE SYNTHESIS --'

echo ''

echo ' #####   ######  ##  ##   ####   ##  ##  ##   ##   ####   #####   ##  ##  ######  ##  ##   ####  '
echo ' ##  ##  ##      ### ##  ##  ##  ##  ##  ### ###  ##  ##  ##  ##  ## ##     ##    ### ##  ##     '
echo ' #####   ####    ## ###  ##      ######  ## # ##  ######  #####   ####      ##    ## ###  ## ### '
echo ' ##  ##  ##      ##  ##  ##  ##  ##  ##  ##   ##  ##  ##  ##  ##  ## ##     ##    ##  ##  ##  ## '
echo ' #####   ######  ##  ##   ####   ##  ##  ##   ##  ##  ##  ##  ##  ##  ##  ######  ##  ##   ####  '
echo '                                                                                                 '
echo '                  ####    ####   ##   ##  #####   ##      ######  ######  ######                 '
echo '                 ##  ##  ##  ##  ### ###  ##  ##  ##      ##        ##    ##                     '
echo '                 ##      ##  ##  ## # ##  #####   ##      ####      ##    ####                   '
echo '                 ##  ##  ##  ##  ##   ##  ##      ##      ##        ##    ##                     '
echo '                  ####    ####   ##   ##  ##      ######  ######    ##    ######                 '

echo ''
