cat calendar.dat | while IFS="," read -a array; do wget ${array[1]}; ical2org < basic.ics > "${array[0]}.org"; rm basic.ics; done;
cat calendar.dat | while IFS="," read -a array; do mv "${array[0]}.org" c:/Users/beyeran/Dropbox/org/calendar; done;
