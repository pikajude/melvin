# config valid only for Capistrano 3.1
lock '3.1.0'

set :application, 'damn'
set :repo_url, 'https://github.com/joelteon/melvin.git'

ROOT = '/app/irc/damn'
set :deploy_to, ROOT

set :linked_dirs, %w{.cabal-sandbox}

set :log_level, :debug

namespace :deploy do
  desc 'Install dependencies'
  task :install do
    on roles(:app), in: :sequence do
      execute :cabal, "update"
      within "#{ROOT}/current" do
        execute(:cabal, "sandbox", "init")
        execute(:cabal, "install", "--only-dependencies", "--force-reinstalls", *fetch(:flags))
        execute(:cabal, "clean")
        execute(:cabal, "configure", *fetch(:flags))
      end
    end
  end

  desc 'Build'
  task :build do
    on roles(:app), in: :sequence do
      within "#{ROOT}/current" do
        execute(:cabal, "build")
      end
    end
  end

  desc 'Restart application'
  task :restart do
    on roles(:app), in: :sequence, wait: 5 do
      execute "sudo stop damn || true"
      sudo :start, "damn"
    end
  end

  after :publishing, :install
  after :install, :build
  after :build, :restart
end
