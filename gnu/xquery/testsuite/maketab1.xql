declare xmlspace = preserve
let $newline := "
",
$result := (document("tab.xml")/result)
  return
    (<table>
{for $x in ($result/row)
      return (<tr>{
        for $y in ($x/fld1) return (<td><b>{children($y)}</b></td>),
        for $y in ($x/fld2) return (<td>{list(100,children($y))}</td>)}</tr>,
        $newline)
}</table>,$newline)
