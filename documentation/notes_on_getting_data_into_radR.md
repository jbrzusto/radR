Tracks are recorded as simple .csv files with each row corresponding
to a "blip" that forms part of a track.  However, radR only writes
these files, and is unable to read them.  If you generate tracks in
this format, any code Phil or others have for post-processing tracks
should work, but users won't be able to do anything in radR with those
tracks.

"blipmovies" record "blips" (contiguous patches of "significantly
above background" (SABG) ADC samples) from a sequence of sweeps.  radR
reads and writes these, and can perform filtering and track
construction from them.  This format was meant to represent full
information about putative targets, for subsequent filtering and track
assembly.  I think it would be a major, challenging, and frustrating
effort to independently write code to read or write this format, even
with the full source of the current implementation available.  It is
awkward, not robust, not easily extended, and relies to a great extent
on R internals and on some custom data structures which don't
represent radar signals in a physically realistic way.  And there is
just too much code that isn't used anywhere else (i.e. there's no
use/support ecosystem).

So I'm passing along my quick, best description of the format, but I
am extremely reluctant to spend further time on any efforts to
reimplement it in a new context.  There are other ways to get data
into radR, such as raw archives or indeed mpeg movies, which would
require a much smaller investment on your part.  These are full-sweep
inputs that would then rely on radR's own target-finding algorithms.
And a completely new, sqlite-based disk-format using a cleaner
target schema would almost certainly be a better use of your time,
even though you would have to write a radR plugin to read/write that
new format if anyone were to want to use radR with it.

# On-disk format of tracks.csv #

Blips are assembled into tracks using the multi-frame correspondence
model.  Tracks deemed "complete" (i.e. the target has not been seen in
a while) are written to a "tracks.csv" file.  Columns in the track
.csv files are as listed in the code here:

   https://github.com/jbrzusto/radR/blob/master/plugins/tracker/tracker.conf.R

in lines 58-90

The meanings of most of the columns in track files are described here:

   https://radr-project.org/03_-_Current_Features_and_Plug-ins/Filtering_blips_using_an_R_expression?highlight=blips

Here are descriptions of the remaining fields

  scan.no: index of which sweep this blip was seen (within the source
           from which the tracks were generated, e.g. a blip-movie)
           A given track can contain at most one line per sweep.

  track.no: index of the track to which this blip has been assigned
            A "track" consists of all rows in the file with the same
            value of track.no
            
  blip.no: index of this blip among all blips found while building the 
           tracks.

# On-disk format of blipmovies #

Blips are recorded in a **supremely awkward** format called
"blipmovies".  At the lowest level, it uses the serialization format
from R itself, which is poorly documented (really, it is documented by
the read/write code itself).  At the mid level, it consists of a
sequence of arbitrary R objects written to disk along with an index
file to permit random access.  At the top level, each sweep is
recorded as some metadata (but only that which changed from the
previous sweep), and then a sparse representation of "patches";
i.e. sets of SABG samples which are contiguous in pulse x range-cell
space, and satisfy some user-select filtering criteria as to number of
samples, area, etc.)  The code uses the terms "patches" and "blips",
with the latter being simply a filtered subset of the former.

## Lowest Level: serialized R objects ##

Unfortunately, the R serialization format is not well-documented (at least
last time I looked).  It is inherent in the code here:

   https://github.com/wch/r-source/blob/trunk/src/main/serialize.c
   
If you are going to write code to generate blipmovies, which I strongly
recommend against, I also strongly recommend that you simply compile
and use a copy of R as a shared library, and call that serialization
code directly, rather than trying to imitate it.  Of course, that requires
wrapping your in-memory data into R structures.

## Mid Level: a random-access list of R objects ##

The serialized "R" objects are recorded in a `biglist`, which permits
random access.  The code for creating and using these lists is in the
two R packages here:

  https://github.com/jbrzusto/radR/tree/master/packages/bigframe

  - this manages a "data.frame" of a fixed set of columns but arbitrary 
    number of rows.  It is used by "biglist" as an index into the file
    of serialized R objects.

  https://github.com/jbrzusto/radR/tree/master/packages/biglist

  - this manages a list of an arbitrary number of arbitrary R objects.
  The disk offset and size (both in bytes) of the n'th object is
  stored in row `n` of a bigframe.  So a biglist consists of two files
  on disk:
  
    - the ".bm" file contains the serialized R objects in the list,
      one after another with no delimiters, in the order in which they
      were assigned to slots, rather than in the order of the list
      (i.e. objects can be assigned to an arbitrary slot in the list
      without creating a gap in this file.  e.g. the sequence
         x[[10]] <- Obj1; x[[2]] <- Obj2;
      will store the serialized Obj2 after the serialized Obj1.
      
    - the ".bm.i" file is an index into the ".bm" file.  After the
      header, the location and size of the n'th object in the ".bm"
      file is stored as a pair (offset: 8 byte little-endian double,
      size: 8 byte little-endian double) at location `sizeof(HEADER) +
      16 * (n-1)` of the ".bm.i" file.  This index creates an (offset,
      size) pair for each slot in the biglist, up to the maximum slot
      assigned so far.  For unassigned slots before that maximum,
      the `offset` field is a NaN.

## Top Level: sweep metadata and data as R objects ##

What is stored in the blipmovie "biglist" is a sequence of
objects described here:

  https://github.com/jbrzusto/radR/blob/master/plugins/blipmovie/DESIGN

And even there, the objects being stored are themselves complicated.
For each sweep, a "run buffer" is saved, which amounts to a linked
list of consecutive runs (within a pulse) of SABG samples.  It is a
sparse representation of that part of the sweep deemed to have signal
from "targets" rather than from noise or background.

Details on the format of that structure are here:

   https://github.com/jbrzusto/radR/blob/master/include/patchify.h

The code which "patchifies" a raw sweep buffer is here:

   https://github.com/jbrzusto/radR/blob/master/main/patchify.c
