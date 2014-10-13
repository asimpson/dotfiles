<?php
  //~/.tmux-harvest.php
  // should have three variables
  // $email, $company, and $password
  $home = $_SERVER["HOME"];
  require("$home/.tmux-harvest.php");

  $credentials = "$email:$password";
  $url = "https://$company.harvestapp.com/daily/";

  $headers = array(
    "Content-type: application/json",
    "Accept: application/json",
    "Authorization: Basic " . base64_encode($credentials)
  );

  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, $url);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
  curl_setopt($ch, CURLOPT_TIMEOUT, 60);
  curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);

  $resp = curl_exec($ch);
  $data = json_decode($resp, true);

  if (curl_errno($ch)) {
    print "Error: " . curl_error($ch);
  } else {
    curl_close($ch);
    $today = $data["day_entries"];
    $timer_running = false;

    if ( empty($today) ) {
      echo "No timer running";
      return;
    }

    foreach($data["day_entries"] as $entry) {
      $project = $entry["project"];
      $formattedProject = (strlen($project) > 15) ? substr($project,0,15).'[...]' : $project;

      if ( array_key_exists("timer_started_at", $entry) ) {
        $timer_running = true;
        $hour = $entry["hours"];
        echo "$formattedProject - $hour";
      } 
    }

    if ( $timer_running == false ) {
      echo "No timer running";
    }
  }
?>
