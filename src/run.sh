#!/bin/bash
echo
echo t/t1.pl
./t1.sh # swipl -s ./run.pl -g "t1,halt."
echo
echo t/t2.pl
./t2.sh # swipl -s ./run.pl -g "t2,halt."
echo
echo t/t3.pl
./t3.sh # swipl -s ./run.pl -g "t3,halt."
echo
echo t/t4.pl
./t4.sh # swipl -s ./run.pl -g "t4,halt."
echo
echo t/t5.pl
./t5.sh # swipl -s ./run.pl -g "t5,halt."
echo
echo t/t6.pl
./t6.sh # swipl -s ./run.pl -g "t6,halt."
echo
echo t/t7.pl
./t7.sh # swipl -s ./run.pl -g "t7,halt."

echo
echo t/t20.pl
./t20.sh # swipl -s ./run.pl -g "t20,halt."

echo
echo t/t22.pl
./t22.sh # swipl -s ./run.pl -g "t22,halt."
echo
echo t/t23.pl
./t23.sh # swipl -s ./run.pl -g "t23,halt."
echo
echo t/t24.pl
./t24.sh # swipl -s ./run.pl -g "t24,halt."
