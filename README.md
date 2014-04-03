# 沢庵(Takuan)

So you want to load Python pickles in Clojure? Well, you see, pickles are a
foreign semi-binary format and there's no native way to load them. In fact,
since they serialize arbitrary Python objects it's not even clear what loading
a pickle outside of Python means. Looks like you're in a bit of a... pickle.

But don't dispair, you might not be able to have proper pickles, but at least
you can have [沢庵](http://en.wikipedia.org/wiki/Takuan)! 沢庵 will load any
Python primative into the analagous Clojure type. Objects will be shoehorned
into maps with some extra jazz as best as it possible under the circumstances.
You're on your own for methods though. If the moons align recursive structures
should work. Mostly. Kinda.

## License
Copyright (C) 2014 Ryan (Lanny) Jenkins

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
