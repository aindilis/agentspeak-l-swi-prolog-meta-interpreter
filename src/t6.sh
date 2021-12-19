#!/bin/bash

swipl -s agentspeak-l.pl  -g "consult('t/t6'),start_agent,solve(!testFailureHandlingMechanism),halt."
