package gnu.expr;

public class SwitchExp extends Expression
{
  Expression value;

  CaseClause clauses;

  Expression defaultCase;
}

class CaseClause
{
  CaseClause next;

  Expression[] values;

  Expression body;
}
