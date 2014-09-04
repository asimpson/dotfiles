#!/usr/bin/ruby 
require 'date'
require 'fileutils'
 
desktop = "/Users/asimpson/Desktop/"
dest = "/Users/asimpson/Inbox/"
 
Dir.foreach(desktop) do |file|
  next if File.fnmatch?('.*', file, File::FNM_DOTMATCH) or file == 'old-downloads'
 
  Dir.chdir(desktop)
  fileTime = File.atime(file)
  timeNow = Time.now
 
  if fileTime < (timeNow - 14400)
    filePath = "#{desktop}#{file}"
    `/usr/local/bin/tag -a inbox '#{file}'`
    FileUtils.mv(file, dest, {:verbose => true})
  end
end
