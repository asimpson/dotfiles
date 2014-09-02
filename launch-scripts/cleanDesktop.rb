#!/usr/bin/ruby 
require 'date'
require 'fileutils'
 
desktop = "/Users/asimpson/Desktop/"
oldDownloads = "/Users/asimpson/Desktop/old-downloads/"
dest = "/Users/asimpson/Inbox/"
 
Dir.foreach(desktop) do |file|
  next if file == '.' or file == '..' or file == 'old-downloads'
 
  Dir.chdir(desktop)
  fileTime = File.atime(file)
  timeNow = Time.now
 
  if fileTime < (timeNow - 14400)
    filePath = "/Users/arthursimpson/Desktop/#{file}"
    `/usr/local/bin/tag -a inbox #{filePath}`
    FileUtils.mv(filePath, dest, {:verbose => true})
  end
end
