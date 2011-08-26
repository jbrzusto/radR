BEGIN {
  inside = 0;
}

/^(typedef)?[ \t]+(struct|union)/ {
  if (inside == 0) {
    split ("", members, "");
    split ("", linemember, "");
    n = 0;
    m = 0;
    printf("prev_offset = 0; prev_sizeof = 0;\n")
  }
  lines[++m] = gensub(/\"/, "\"", "g", $0);
  line = gensub(/\"/, "\"", "g", $0);
  ++inside;
  next;
}

/^[ \t]*}/ {
lines[++m] = gensub(/\"/, "\"", "g", $0);
--inside;
if (inside == 0) {
lines[m] = gensub(/}/, "} MS_STRUCT ", "", lines[m]);
$0 = gensub(/[ \t]*}[ \t]*/, "", "g", $0);
  split($1, typename, /[,;]/);
for (j=1; j <= m; ++j) {
if (linemember[j]) {
i = linemember[j];
      printf("   offset = offsetof(" typename[1] ", " members[i] ");\n");
printf("   if (offset > prev_offset + prev_sizeof)" );
printf("      printf(\"  MS_STRUCT_FILLER(%%d, %%d);\\n\", ++i, offset - (prev_offset + prev_sizeof));\n");
printf("prev_offset = offset; prev_sizeof = mysizeof(" typename[1] "," members[i] ");\n");
  }
 printf("puts(\"" lines[j] "\");\n");
}
}
  next;
}

inside {
lines[++m] = gensub(/"/, "\"", "g", $0);
  $0 = gensub(/\/\/.*$/, "", "g", $0);
  if ($0 ~ /:[ 0-9;]*$/)
     next;
  split($NF, A, "[[;]");
     if (! (A[1] ~ /^[ \t}{]*$/)) {
  members[++n] = A[1];
linemember[m] = n;
}
next;
}

{
line = gensub(/\"/, "\"", "g", $0);
 printf("puts(\"" line "\");\n");
}
