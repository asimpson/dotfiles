#!/usr/bin/env ruby
$host = 'rsync'
$repo = 'laptop'
# Define exclusions here.
# https://borgbackup.readthedocs.io/en/latest/usage/help.html#borg-help-patterns
$exclude_file = "#{Dir.home}/.borg-exclude.txt"
# This is a gpg encrypted file that contains the repo password.
$password_file = "#{Dir.home}/.borg-rsync-pass.gpg"

def run_borg
  dotfiles = Dir.glob(File.expand_path('~/.*'))
               .select { |f| f[-1] != "." }
               .join " "
  projects = "/home/asimpson/Projects"
  etc = "/etc"

  ENV['BORG_PASSCOMMAND'] = "gpg -dq #{$password_file}"
  puts "breaking lock if there is one..."
  `borg break-lock --remote-path=borg1 #{$host}:#{$repo}`
  puts "starting borg create..."
  `borg create --remote-path=borg1 -s --exclude-from #{$exclude_file} #{$host}:#{$repo}/::{now} #{dotfiles} #{etc} #{projects}`
  puts "pruning..."
  `borg prune --remote-path=borg1 -s --keep-daily 10 --keep-monthly 6 #{$host}:#{$repo}`
end

puts "Starting new backup..."
run_borg
