// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;

public abstract class XPathVariable
{
    public abstract boolean getBooleanValue();
    public abstract double getNumberValue();
    public abstract void result( XPathContext context, int resultType );
}
