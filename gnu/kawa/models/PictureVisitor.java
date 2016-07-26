package gnu.kawa.models;

public class PictureVisitor {
    public void visitFillShape(FillShape pic) {
    }
    public void visitDrawShape(DrawShape pic) {
    }
    public void visitDrawImage(DrawImage pic) {
    }
    public void visitWithPaint(WithPaint pic) {
        pic.picture.visit(this);
    }
    public void visitWithTransform(WithTransform pic) {
        pic.picture.visit(this);
    }
    public void visitWithComposite(WithComposite pic) {
        for (Picture child : pic.children)
            child.visit(this);
    }
    public void visitPBox(PBox pic) {
        for (Picture child : pic.children)
            child.visit(this);
    }
}
