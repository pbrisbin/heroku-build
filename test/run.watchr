# vim:ft=ruby:
watch( '.*\.hs'              ) { system("cabal test") }
watch( 'heroku-build\.cabal' ) { system("cabal test") }
