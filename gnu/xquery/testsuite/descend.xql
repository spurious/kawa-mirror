declare function descendent-or-self ($x) {
  $x, for $z in children($x) return descendent-or-self($z) };
descendent-or-self (<a>text1<b>text2</b></a>)
