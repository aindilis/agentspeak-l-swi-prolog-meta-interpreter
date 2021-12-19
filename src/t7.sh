#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t7'),start_agent,solve(!is_good_job(9000,cashier)),solve(!is_good_job(20000,doctor)),solve(!find_best_job),halt."
