# vim: set syntax=ruby

guard :shell do
  watch %r{src/*/.*\.hs} do |m|
    puts "\n\n\n\n"
    puts "=[ #{m[0]} ]===================================="
    puts "-[ Building ]-----------------------------------"
    if system("cabal build") then
      `./dist/build/evil/evil`
    end
  end
end
