let $x:=12,
    $y:=<a>{$x+$x}</a>
  return <b atr1='11' atr2="{$x}">{($y,99,$y)}</b>
