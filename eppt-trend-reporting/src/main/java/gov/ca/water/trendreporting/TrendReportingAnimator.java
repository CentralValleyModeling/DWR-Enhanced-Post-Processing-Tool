/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.trendreporting;

import java.util.HashMap;
import java.util.Map;

import javafx.animation.Transition;
import javafx.beans.property.DoubleProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.transform.Translate;
import javafx.util.Duration;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class TrendReportingAnimator implements ChangeListener<Number>, ListChangeListener<Node>
{

	private Map<Node, MoveXTransition> _nodeXTransitions = new HashMap<>();
	private Map<Node, MoveYTransition> _nodeYTransitions = new HashMap<>();

	void observe(ObservableList<Node> nodes)
	{
		for(Node node : nodes)
		{
			this.observe(node);
		}
		nodes.addListener(this);
	}


	private void observe(Node n)
	{
		n.layoutXProperty().addListener(this);
		n.layoutYProperty().addListener(this);
	}

	private void unobserve(Node n)
	{
		n.layoutXProperty().removeListener(this);
		n.layoutYProperty().removeListener(this);
	}

	@Override
	public void changed(ObservableValue<? extends Number> ov, Number oldValue, Number newValue)
	{
		final double delta = newValue.doubleValue() - oldValue.doubleValue();
		final DoubleProperty doubleProperty = (DoubleProperty) ov;
		final Node node = (Node) doubleProperty.getBean();

		if("layoutX".equals(doubleProperty.getName()))
		{
			MoveXTransition tx = _nodeXTransitions.get(node);
			if(tx == null)
			{
				tx = new MoveXTransition(node);
				_nodeXTransitions.put(node, tx);
			}
			tx.setFromX(tx.getTranslateX() - delta);

			tx.playFromStart();
		}
		else
		{
			// "layoutY"
			MoveYTransition ty = _nodeYTransitions.get(node);
			if(ty == null)
			{
				ty = new MoveYTransition(node);
				_nodeYTransitions.put(node, ty);
			}
			ty.setFromY(ty.getTranslateY() - delta);

			ty.playFromStart();
		}
	}

	private abstract class MoveTransition extends Transition
	{

		final Translate _translate;

		MoveTransition(final Node node)
		{
			final Duration movementAnimationDuration = new Duration(500);
			setCycleDuration(movementAnimationDuration);
			_translate = new Translate();

			node.getTransforms().add(_translate);
		}

		double getTranslateX()
		{
			return _translate.getX();
		}

		double getTranslateY()
		{
			return _translate.getY();
		}
	}

	private final class MoveXTransition extends MoveTransition
	{
		private double _fromX;

		private MoveXTransition(final Node node)
		{
			super(node);
		}

		@Override
		protected void interpolate(double frac)
		{
			_translate.setX(_fromX * (1 - frac));
		}

		private void setFromX(double fromX)
		{
			_translate.setX(fromX);
			this._fromX = fromX;
		}
	}

	private final class MoveYTransition extends MoveTransition
	{
		private double _fromY;

		private MoveYTransition(final Node node)
		{
			super(node);
		}

		@Override
		protected void interpolate(double frac)
		{
			_translate.setY(_fromY * (1 - frac));
		}

		private void setFromY(double fromY)
		{
			_translate.setY(fromY);
			this._fromY = fromY;
		}
	}

	@Override
	public void onChanged(Change<? extends Node> change)
	{
		while(change.next())
		{
			if(change.wasAdded())
			{
				for(Node node : change.getAddedSubList())
				{
					this.observe(node);
				}
			}
			else if(change.wasRemoved())
			{
				for(Node node : change.getRemoved())
				{
					this.unobserve(node);
				}
			}
		}
	}

}
