(in-package :cl-user)
(require 'asdf)
(asdf:defsystem :skyline-tool
  :description "A tool for building tile-based adventure games for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.9.0"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@star-hope.org"
  :licence "MIT" ; if this poses a problem, ask me for a waiver.
  :long-name "The Skyline tools for building ARPG's for Atari 2600 and more"
  
  :depends-on ( ;; broken into lines for easier sorting
               :alexandria
               :bordeaux-threads
               :cl-6502
               :cl-base64
               :cl-change-case
               :cl-ppcre 
               :clim-listener
               :clim-debugger
               :climacs
               :clouseau
               :cserial-port
               :dufy
               :mcclim
               :lparallel
               :midi
               :parse-number
               :png-read
               :quicklisp-slime-helper
               :replic
               :split-sequence
               :swank
               :trivial-gray-streams
               :xmls
               :yacc
               :zip
               )
  
  :encoding :utf-8
  
  :serial t
  
  :components
  ((:file "gray-streams-pipe")
   ;;(:file "clim-simple-interactor")
   (:module "src"
    :components ((:file "package")
                 (:file "utils")
                 (:file "misc")
                 (:file "assembly")
                 (:file "7800gd-debug")
                 (:file "7800gd-interface"
                  :depends-on ("7800gd-debug" "eprom"))
                 (:file "music")
                 (:file "eprom")
                 (:file "fountain")
                 (:file "graphics")
                 (:file "maps")
                 (:file "oops")
                 (:file "i18n-l10n")
                 (:file "listings")
                 (:file "skylisp")
                 (:file "tables")
                 (:file "threed")
                 (:file "asset-allocator")
                 (:file "interface")))))
