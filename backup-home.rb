#!/usr/bin/env ruby
$prefix = 'com.apple.TimeMachine.'
$host = 'trisolaris'
$repo = '/mnt/crate/backups/adam-air'
# Where the snapshot will be mounted.
$mount = "#{Dir.home}/borg"
$ref_file = "#{Dir.home}/.borg-nas-snapshot"
# Define exclusions here.
# https://borgbackup.readthedocs.io/en/latest/usage/help.html#borg-help-patterns
$exclude_file = "#{Dir.home}/.borg-exclude.txt"
# This is a gpg encrypted file that contains the repo password.
$password_file = "#{Dir.home}/.borg-nas-pass.gpg"

def record_snapshot snapshot
  File.new($ref_file, "w")
  File.open($ref_file, "w") { |f| f.write snapshot }
end

def cleanup
  puts "Cleaning up after the borg..."
  ref = File.read($ref_file)
  `diskutil umount #{$mount}`
  `tmutil deletelocalsnapshots #{ref}`
  File.delete($ref_file)
end

def run_borg
  home = "#{$mount}/Users/asimpson/"

  ENV['BORG_PASSCOMMAND'] = "gpg -dq #{$password_file}"
  puts "breaking lock if there is one..."
  `borg break-lock #{$host}:#{$repo}`
  puts "starting borg create..."
  `borg create --list -s --exclude-from #{$exclude_file} #{$host}:#{$repo}/::{now} #{home}`
  puts "pruning..."
  `borg prune -s --keep-daily 10 --keep-monthly 6 #{$host}:#{$repo}`
end

def new
  snapshot = `tmutil localsnapshot`.split(':')[1].strip
  `diskutil umount #{$mount}`
  `mount_apfs -s #{$prefix}#{snapshot} / #{$mount}`
  record_snapshot snapshot
end

existing_snapshot_file = File.file?($ref_file) && File.read($ref_file)
local_snapshots = !`tmutil listlocalsnapshots /`.empty?

if existing_snapshot_file and local_snapshots then
  puts "Found existing snapshot. Resuming..."
  `mount_apfs -s #{$prefix}#{existing_snapshot_file} / #{$mount}`
else
  puts "Starting new backup..."
  new
end

run_borg
cleanup
