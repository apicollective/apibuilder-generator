#!/usr/bin/env ruby

# Download the ruby client from apidoc
load 'ruby_client.txt'

client = Com::Gilt::Apidoc::Example::Union::Types::V0::Client.new("http://localhost:9003")

def extract_guid(user)
  if user.respond_to?(:guid)
    user.guid
  elsif user.respond_to?(:value)
    user.value
  else
    raise "Cannot find guid for user: " + user.inspect
  end
end

puts "Fetching all users"
guids = client.users.get.map do |u|
  puts u.inspect
  extract_guid(u)
end

guids.each do |guid|
  puts "Fetching user with guid[#{guid}]"
  u = client.users.get_by_guid(guid)
  puts "  --> Got user with guid: #{extract_guid(u)}"
end


puts "Creating a guest user"
guid = "f3973f60-be9f-11e3-b1b6-0800200c9a67"
user = Com::Gilt::Apidoc::Example::Union::Types::V0::Models::GuestUser.new(:guid => guid, :email => "#{guid}@gilttest.com")
puts user.to_json
client.users.post(user)

puts "Creating a registered user"
guid = "f3973f60-be9f-11e3-b1b6-0800200c9a66"
user = Com::Gilt::Apidoc::Example::Union::Types::V0::Models::RegisteredUser.new(:guid => guid, :email => "#{guid}@gilttest.com", :preference => Com::Gilt::Apidoc::Example::Union::Types::V0::Models::Foo.a)
puts user.to_json
client.users.post(user)

puts "Creating a user uuid wrapper"
guid = "f3973f60-be9f-11e3-b1b6-0800200c9a65"
user = Com::Gilt::Apidoc::Example::Union::Types::V0::Models::UserUuidWrapper.new(:value => guid)
puts user.to_json
client.users.post(user)

puts "DONE"
