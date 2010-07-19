;; kawa:scheme
(define (num-parameter name default)
  (string->number (request-parameter name default)))

#<html>
  <head><title>Accumulating Adder</title></head>

  <body>
    <form>
      <table>
        <tr>
          <td>Result so far: </td>
          <td>
            <input
              name="sum1"
              value="&(+ (num-parameter "sum1" 0)
                         (num-parameter "sum2" 0))" />
          </td>
        </tr>
        <tr>
          <td>Add to it:</td>
          <td>
            <input
              name="sum2"
              value="&(num-parameter "sum2" 1)" />
          </td>
        </tr>
      </table>
      <input type="submit" value="Submit" />
    </form>
  </body>
</html>
