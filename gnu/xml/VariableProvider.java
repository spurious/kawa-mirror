// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

public interface VariableProvider
{
    public XPathVariable getVariable( Object qname );
    public void setVariable( Object qname, XPathVariable value );
}
