#!/bin/bash

swipl -s agentspeak-l.pl -g "consult('parser.pl'),start_agent,load_agent('t/t25.pasl'),solve(!print_fact(5)),halt."
