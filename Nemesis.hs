import Prelude hiding ((-))
import System.Nemesis
import System.Nemesis.Utils ((-))
import Control.Monad
import Data.Monoid
import Data.Time

main = run nemesis


runP :: String -> IO ()
runP = cabal . ("--forbidden-ip '' " <>)

sleep :: Int -> IO ()
sleep x = sh - "sleep " <> show x


cabal :: String -> IO ()
cabal x = do
  let cmd = "cabal cabal -- " <> x
  putStrLn cmd
  sh cmd

nemesis = do
  namespace "build" - do
    task "x86-64" - do
      sh "cabal install --program-suffix='-x86-64'"

    task "armhf" - do
      sh "cabal install --program-suffix='-armhf'"

    task "darwin-x86-64" - do
      sh "cabal install --program-suffix='-darwin-x86-64'"

    task "hash" - do
      sh - "cd .cabal-sandbox/bin; sha256sum "
          <> "moesocks-armhf"
          <> " "
          <> "moesocks-x86-64"
          -- <> "moesocks-darwin-x86-64"

    task "hash-mac" - do
      sh - "cd .cabal-sandbox/bin; shasum -a 256 "
          <> "moesocks-darwin-x86-64"

    let tar x y =
          sh - "cd .cabal-sandbox/bin;"
                <> "tar -c -J -f " <> "../../dist/" <> x <> " " <> y

    task "compress-x86-64" - do
      tar "moesocks-x86-64.tar.xz" "moesocks-x86-64"

    task "compress-armhf" - do
      tar "moesocks-armhf.tar.xz"  "moesocks-armhf"

    task "compress-darwin-x86-64" - do
      tar "moesocks-darwin-x86-64.tar.xz"  "moesocks-darwin-x86-64"

    task "compress:compress-x86-64 compress-armhf compress-darwin-x86-64" -
      return ()


  desc "halive"
  task "h" - do
    sh "halive src/HaliveMain.hs src"

  namespace "bench" - do
    desc "run"
    task "run" - do
      runP "-r debug -c bench/bench.json -v"

    desc "run-r"
    task "run-r" - do
      runP "-r remote -c bench/bench.json -v"

    desc "run-l"
    task "run-l" - do
      runP "-r local -c bench/bench.json -v"

    desc "run-ss-r"
    task "run-ss-r" - do
      sh "ssserver -c bench/bench.json --forbidden-ip='' -v"

    desc "run-ss-l"
    task "run-ss-l" - do
      sh "sslocal -c bench/bench.json -v"

    desc "run-ss-libev-r"
    task "run-ss-libev-r" - do
      sh "ss-server -c bench/bench.json -v"

    desc "run-ss-libev-l"
    task "run-ss-libev-l" - do
      sh "ss-local -c bench/bench.json -v"

    desc "http"
    task "http" - do
      cabal "-r debug -c bench/bench.json -T 8081:127.0.0.1:8080"

    let url = "http://127.0.0.1:8080/test.mp4"
        urlSmall = "http://127.0.0.1:8080/"
        cmd = "parallel --no-notice curl -x socks5://127.0.0.1:1091 -o /dev/null"
        args _url = concatMap (<> " ") - replicate 4 _url

    desc "load"
    task "load" - do
        forever-
          sh - "curl -x socks5://127.0.0.1:1091 -o /dev/null"
              <> " "
              <> url

    desc "parallel-one"
    task "parallel-one" - do
      sh - cmd
            <> " ::: "
            <> args url

    desc "parallel"
    task "parallel" - do
      forever -
        sh - cmd
              <> " ::: "
              <> args url

    desc "parallel-small"
    task "parallel-small" - do
      forever -
        sh - cmd
              <> " ::: "
              <> args urlSmall

  desc "test port forwarding with a tcp dns request"
  task "dig-tcp" - do
    sh "dig @127.0.0.1 -p 5300 +tcp twitter.com"

  desc "test port forwarding with a loop of udp dns request"
  task "dig" - do
    forever - do
      sh "dig @127.0.0.1 -p 5300 twitter.com; exit 0;"
      sleep 1

  desc "test port forwarding with a udp dns request"
  task "dig-one" - do
    sh "dig @127.0.0.1 -p 5300 twitter.com; exit 0;"
    sleep 1

  desc "dist"
  task "dist" - do
    sh "cabal clean"
    sh "cabal configure"
    sh "cabal sdist"

  desc "remote"
  task "remote" - do
    cabal "-r remote -c dev.json"

  desc "remote-o"
  task "remote-o" - do
    cabal "-r remote -o -c dev.json"

  let commonOptions = " -U 5300:8.8.8.8:53 -T 5300:8.8.8.8:53 "
                      -- <> "--tcp-buffer-size 256"

      _c = commonOptions

  desc "local-profile"
  task "local-profile" - do
    runP - "-r local -c dev.json" <> _c

  desc "local"
  task "local" - do
    cabal - "-r local -c dev.json" <> _c

  desc "local-o"
  task "local-o" - do
    cabal - "-r local -o -c dev.json" <> _c

  desc "remote-debug"
  task "remote-debug" - do
    cabal - "-r remote -c dev.json -v"

  desc "local-debug"
  task "local-debug" - do
    cabal - "-r local -c dev.json -v" <> _c

  desc "local2"
  task "local2" - do
    cabal - "-r local -c dev2.json" <> _c

  desc "local2-o"
  task "local2-o" - do
    cabal - "-r local -o -c dev2.json" <> _c

  desc "local3"
  task "local3" - do
    cabal - "-r local -c dev3.json" <> _c

  desc "i"
  task "i" - do
    sh "cabal repl"

  desc "local-debug-clean"
  task "local-debug-clean" - do
    cabal - "-r local -c dev-debug.json -v -T "
        <> " 5300:[2001:4860:4860::8888]:53"

  desc "remote-debug-clean"
  task "remote-debug-clean" - do
    cabal "-r remote -c dev-debug.json -v"


  desc "merge debug to master"
  task "m" - do
    sh "git checkout master"
    sh "git merge dev -r \"Merge branch 'debug'\""
    sh "git push origin master"
    sh "git checkout dev"


  task "gen-profile" - do
    sh "hp2ps moesocks.hp"
    time <- getCurrentTime
    let timeStr =  formatTime  defaultTimeLocale
                      (iso8601DateFormat (Just "-%H-%M-%S") ) time
    let newPath = "moesocks-" <> timeStr <> ".hp"

    sh - "mv moesocks.ps " <> newPath
    sh - "rm moesocks.aux"
    sh - "evince " <> newPath


  namespace "angel" - do
    desc "remote"
    task "remote" - do
      sh "angel angel/remote.conf"

    desc "local"
    task "local" - do
      sh "angel angel/local.conf"

    desc "local2"
    task "local2" - do
      sh "angel angel/local2.conf"



  desc "tcp metrics"
  task "metrics" - do
    sh "ip tcp_metrics | grep fo_cookie"
