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

package gov.ca.water.quickresults.ui.dashboard;

import java.util.HashMap;
import java.util.List;
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
public class Animator implements ChangeListener<Number>, ListChangeListener<Node>
{

	private Map<Node, MoveXTransition> nodeXTransitions = new HashMap<>();
	private Map<Node, MoveYTransition> nodeYTransitions = new HashMap<>();

	/**
	 * Animates all the children of a Region.
	 * <code>
	 * VBox myVbox = new VBox();
	 * LayoutAnimator animator = new LayoutAnimator();
	 * animator.observe(myVbox.getChildren());
	 * </code>
	 *
	 * @param nodes
	 */
	public void observe(ObservableList<Node> nodes)
	{
		for(Node node : nodes)
		{
			this.observe(node);
		}
		nodes.addListener(this);
	}

	public void unobserve(ObservableList<Node> nodes)
	{
		nodes.removeListener(this);
	}

	public void observe(Node n)
	{
		n.layoutXProperty().addListener(this);
		n.layoutYProperty().addListener(this);
	}

	public void unobserve(Node n)
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

		switch(doubleProperty.getName())
		{
			case "layoutX":
				MoveXTransition tx = nodeXTransitions.get(node);
				if(tx == null)
				{
					tx = new MoveXTransition(node);
					nodeXTransitions.put(node, tx);
				}
				tx.setFromX(tx.getTranslateX() - delta);

				tx.playFromStart();
				break;

			default: // "layoutY"
				MoveYTransition ty = nodeYTransitions.get(node);
				if(ty == null)
				{
					ty = new MoveYTransition(node);
					nodeYTransitions.put(node, ty);
				}
				ty.setFromY(ty.getTranslateY() - delta);

				ty.playFromStart();
		}
	}

	private abstract class MoveTransition extends Transition
	{
		private final Duration MOVEMENT_ANIMATION_DURATION = new Duration(1000);

		protected final Translate translate;

		public MoveTransition(final Node node)
		{
			setCycleDuration(MOVEMENT_ANIMATION_DURATION);
			translate = new Translate();

			node.getTransforms().add(translate);
		}

		public double getTranslateX()
		{
			return translate.getX();
		}

		public double getTranslateY()
		{
			return translate.getY();
		}
	}

	private class MoveXTransition extends MoveTransition
	{
		private double fromX;

		public MoveXTransition(final Node node)
		{
			super(node);
		}

		@Override
		protected void interpolate(double frac)
		{
			translate.setX(fromX * (1 - frac));
		}

		public void setFromX(double fromX)
		{
			translate.setX(fromX);
			this.fromX = fromX;
		}
	}

	private class MoveYTransition extends MoveTransition
	{
		private double fromY;

		public MoveYTransition(final Node node)
		{
			super(node);
		}

		@Override
		protected void interpolate(double frac)
		{
			translate.setY(fromY * (1 - frac));
		}

		public void setFromY(double fromY)
		{
			translate.setY(fromY);
			this.fromY = fromY;
		}
	}

	@Override
	public void onChanged(Change change)
	{
		while(change.next())
		{
			if(change.wasAdded())
			{
				for(Node node : (List<Node>) change.getAddedSubList())
				{
					this.observe(node);
				}
			}
			else if(change.wasRemoved())
			{
				for(Node node : (List<Node>) change.getRemoved())
				{
					this.unobserve(node);
				}
			}
		}
	}

	// todo unobserving nodes should cleanup any intermediate transitions they may have and ensure they are removed from transition cache to prevent memory leaks.
}
