fname = ARGV[0]
puts "name is #{fname}"
puts `clear`
puts "-" * 40 
def getTime(name) 
  File.mtime(name)
end
system "polyc #{fname}"
mtime = getTime(fname)
loop do 
 if mtime != getTime(fname)
  puts `clear`
  system "polyc #{fname}"
 end
  mtime = getTime(fname)
  sleep(3)
end
