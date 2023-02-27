import matplotlib.pyplot as plt
import matplotlib.patches as patches


def plot_points_and_rectangles(points, rectangles):
    fig, ax = plt.subplots()

    # plot points
    x_points = [p[0] for p in points]
    y_points = [p[1] for p in points]
    ax.plot(x_points, y_points, 'ro')

    # plot rectangles
    for r in rectangles:
        rect = patches.Rectangle((r[0], r[1]), r[2]-r[0], r[3]-r[1], linewidth=1, edgecolor='r', facecolor='none')
        ax.add_patch(rect)

    plt.show()
