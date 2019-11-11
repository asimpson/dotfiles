const shell = require('child_process').execSync;
const exec = require('child_process').exec;
//this file contains two strings that query mu like `mu find --fields='f: s' 'NOT blahblah'`.
const queries = require('./mail-check-queries');

const refresh =
  "Refresh | terminal=false bash='mbsync -a; pkill -2 -u $UID mu; sleep 1; mu index --maildir=~/Mail'";

const computedCount = shell(queries.count, '', { shell: true }).toString();
const computedContent = () =>
  new Promise((resolve, reject) => {
    exec(queries.content, { shell: true }, (e, st) => {
      if (e) {
        reject(e);
      }

      resolve(
        st
          .toString()
          .split('\n')
          .map(x => x.replace('<', '&lt;').replace('>', '&gt;'))
      );
    });
  });

computedContent()
  .then(content => {
    if (computedCount !== '0') {
      console.log(
        `:mailbox_with_mail: ${computedCount.trim()} | size=11 usemarkup=false ansi=false`
      );
      console.log('---');
      content.forEach(x => console.log(x));
      console.log('---');
      console.log(refresh);
      console.log(
        "View GNOME Shell Log | bash='journalctl /usr/bin/gnome-shell -f'"
      );
    }
  })
  .catch(() => {
    console.log(':mailbox_with_no_mail: | size=11');
    console.log('---');
    console.log(refresh);
    console.log(
      "View GNOME Shell Log | bash='journalctl /usr/bin/gnome-shell -f'"
    );
  });
