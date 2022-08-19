# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

file(MAKE_DIRECTORY
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src/libvterm"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src/libvterm-build"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/tmp"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src/libvterm-stamp"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src"
  "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src/libvterm-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/calleha/.emacs.d/elpa/vterm-20220613.1614/build/libvterm-prefix/src/libvterm-stamp/${subDir}")
endforeach()
