(<table>{
  for $y in 1 to 10 return (
    <tr>{
      for $x in 1 to 10 return
        let $bg:=(if($x mod 2 + $y mod 2 <= 0) then "lightgreen"
                  else if ($y mod 2 <= 0) then "yellow"
                  else if ($x mod 2 <= 0) then "lightblue"
                  else "white"),
            $prod:=$x*$y
          return <td align="right" bgcolor="{$bg}">{
            if ($x > 1 and $y > 1) then $prod else <b>{$prod}</b>}</td>
    }</tr>,
    "
")
}</table>,"
")

