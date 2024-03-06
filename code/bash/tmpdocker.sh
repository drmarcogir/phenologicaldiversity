docker exec --env year=2009 --env minpoly=1 --env maxpoly=506 marco_g bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
docker exec --env year=2009 --env minpoly=507 --env maxpoly=1013 marco_g12 bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
docker exec --env year=2009 --env minpoly=1014 --env maxpoly=1520 papilio bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
docker exec --env year=2009 --env minpoly=1521 --env maxpoly=1689 dawn bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
docker exec --env year=2009 --env minpoly=1690 --env maxpoly=1858 dusk bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
docker exec --env year=2009 --env minpoly=1859 --env maxpoly=2026 roller bash -c "python3 /home/scripts/phenodiv_zeroversion.py" &
wait
