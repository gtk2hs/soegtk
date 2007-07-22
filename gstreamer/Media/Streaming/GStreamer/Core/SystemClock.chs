-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
module Media.Streaming.GStreamer.Core.SystemClock (
  SystemClock,
  SystemClockClass,
  toSystemClock,
  fromSystemClock,
  castToSystemClock,
  systemClockObtain
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.FFI

{# context lib = "gstreamer" prefix = "gst" #}

systemClockObtain :: IO Clock
systemClockObtain =
    {# call system_clock_obtain #} >>= newClock