fname = ARGV[0]
puts "name is #{fname}"
puts `clear`
puts "-" * 40 
def getTime(name) 
  File.mtime(name)
end
system "polyc #{fname} | head -n 30"
mtime = getTime(fname)
loop do 
 if mtime != getTime(fname)
  puts `clear`
  system "polyc #{fname} | head -n 30"

 end
  mtime = getTime(fname)
  sleep(3)
end
