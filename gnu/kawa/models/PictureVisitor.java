package gnu.kawa.models;

import java.awt.*;
import java.awt.geom.*;

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

    public static class TrackingState extends PictureVisitor {
        protected AffineTransform transform;
        protected Stroke stroke = null;
        protected Paint paint = null;
        protected int strokePropertiesSet = 0;

        public TrackingState(AffineTransform transform) {
            this.transform = transform;
        }
        public TrackingState() {
            this.transform = null;
        }

        @Override
        public void visitWithTransform(WithTransform pic) {
            AffineTransform savedTransform = transform;
            try {
                transform = pic.transform;
                if (transform != null) {
                    transform = new AffineTransform(transform);
                    transform.preConcatenate(savedTransform);
                }
                super.visitWithTransform(pic);
            } finally {
                transform = savedTransform;
            }
        }

        @Override
        public void visitWithPaint(WithPaint pic) {
            Paint savedPaint = this.paint;
            Stroke savedStroke = this.stroke;
            int savedPropertiesSet = this.strokePropertiesSet;
            try {
                if (paint != null)
                    this.paint = pic.paint;
                Stroke nstroke = pic.stroke;
                if (nstroke != null) {
                    strokePropertiesSet |= pic.propertiesSet;
                    if (savedStroke instanceof BasicStroke
                        && nstroke instanceof BasicStroke
                        && (pic.propertiesSet & WithPaint.STROKE_ALL_SET) != WithPaint.STROKE_ALL_SET)
                        nstroke = WithPaint.merge((BasicStroke) nstroke, pic.propertiesSet,
                                        (BasicStroke) savedStroke);
                    this.stroke = nstroke;
                }
                super.visitWithPaint(pic);
            } finally {
                if (paint != null)
                    this.paint = savedPaint;
                if (stroke != null)
                    this.stroke = savedStroke;
                this.strokePropertiesSet = savedPropertiesSet;
            }
        }
    }
}
