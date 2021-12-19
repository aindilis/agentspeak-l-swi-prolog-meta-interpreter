#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('parser.pl'),start_agent,load_agent('t/t23.pasl'),solve(!evaluate_salary),halt."
