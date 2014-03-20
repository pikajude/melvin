server 'joelt.io',
  user: 'deploy',
  roles: %w{app},
  ssh_options: {
    user: 'deploy',
    keys: "#{ENV["HOME"]}/.ssh/deploy",
    port: 13772
  }
