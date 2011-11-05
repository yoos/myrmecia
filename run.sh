#!/bin/bash

cd ../aichallenge/ants

./playgame.py \
    --end_wait=0.25 \
    --verbose \
    --log_dir game_logs \
    --turns 100 \
    --map_file maps/example/tutorial1.map \
    "python dist/sample_bots/python/HunterBot.py" \
    /home/yoos/devel/myrmecia/MyBot







#"python dist/sample_bots/python/LeftyBot.py"
#"python dist/sample_bots/python/HunterBot.py"

