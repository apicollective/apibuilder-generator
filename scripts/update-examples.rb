#!/usr/bin/env ruby

def fetch(name)
  puts ""
  puts "Fetching #{name}"
  tmp = "/tmp/apidoc-generator.update.#{name}.tmp"
  url = "http://apidoc.me/bryzek/#{name}/latest/service.json"
  run "curl --silent '#{url}' | jq . > #{tmp}"
  yield tmp
end

def run(cmd)
  puts "  " + cmd
  system(cmd)
end

def fetch_and_copy(name, target)
  fetch(name) do |path|
    run("cp #{path} #{target}")
  end
end

def fetch_and_copy_to_examples(name)
  fetch(name) do |path|
    run("cp #{path} lib/src/test/resources/examples/#{name}.json")
  end
end

fetch_and_copy_to_examples("apidoc-api")
fetch_and_copy_to_examples("apidoc-generator")
fetch_and_copy_to_examples("apidoc-example-union-types")
fetch_and_copy_to_examples("apidoc-example-union-types-discriminator")

fetch("apidoc-reference-api") do |path|
  run("cp #{path} reference-api/service.json")
  run("cp #{path} lib/src/test/resources/examples/reference-service.json")
end
