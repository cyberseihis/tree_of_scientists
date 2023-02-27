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

poins = [(0.0,1.0),(0.0,4.0),(0.0,4.0),(0.0,1.0),(0.0,1.0),(1.0,1.0),(1.0,2.0),(1.0,2.0),(1.0,4.0),(1.0,0.0),(2.0,0.0),(2.0,4.0),(2.0,0.0),(2.0,2.0),(2.0,2.0),(3.0,0.0),(3.0,8.0),(4.0,0.0),(4.0,5.0),(4.0,7.0),(4.0,0.0),(4.0,1.0),(5.0,2.0),(5.0,0.0),(6.0,2.0),(6.0,0.0),(6.0,0.0),(6.0,0.0),(6.0,0.0),(6.0,0.0),(7.0,0.0),(7.0,8.0),(7.0,8.0),(7.0,1.0),(8.0,5.0),(8.0,3.0),(8.0,4.0),(9.0,0.0),(9.0,2.0),(9.0,5.0),(9.0,1.0),(10.0,0.0),(10.0,0.0),(10.0,13.0),(10.0,5.0),(10.0,1.0),(10.0,1.0),(11.0,2.0),(11.0,0.0),(11.0,0.0),(12.0,0.0),(12.0,4.0),(12.0,0.0),(12.0,11.0),(12.0,1.0),(12.0,4.0),(12.0,0.0),(12.0,4.0),(13.0,2.0),(14.0,9.0),(14.0,0.0),(15.0,0.0),(15.0,5.0),(15.0,1.0),(15.0,0.0),(15.0,0.0),(15.0,0.0),(15.0,3.0),(17.0,0.0),(17.0,5.0),(18.0,0.0),(18.0,0.0),(18.0,5.0),(18.0,2.0),(18.0,1.0),(18.0,6.0),(18.0,2.0),(18.0,6.0),(18.0,0.0),(18.0,6.0),(18.0,0.0),(18.0,2.0),(18.0,0.0),(19.0,0.0),(21.0,6.0),(21.0,7.0),(21.0,4.0),(21.0,0.0),(22.0,2.0),(22.0,2.0),(22.0,0.0),(22.0,2.0),(22.0,6.0),(22.0,3.0),(22.0,1.0),(22.0,1.0),(25.0,0.0)]


rtr = [(0.0,0.0,25.0,13.0),(0.0,0.0,2.0,4.0),(2.0,0.0,4.0,8.0),(4.0,0.0,7.0,7.0),(7.0,0.0,9.0,8.0),(9.0,0.0,11.0,13.0),(11.0,0.0,13.0,11.0),(14.0,0.0,15.0,9.0),(17.0,0.0,18.0,5.0),(18.0,0.0,21.0,7.0),(22.0,0.0,25.0,6.0)]


kdd = [(0.0,0.0,11.0625,13.0),(11.0625,0.0,25.0,13.0),(0.0,0.0,11.0625,2.657142857142857),(0.0,2.657142857142857,11.0625,13.0),(0.0,0.0,5.590909090909091,2.657142857142857),(5.590909090909091,0.0,11.0625,2.657142857142857),(0.0,0.0,5.590909090909091,0.8181818181818182),(0.0,0.8181818181818182,5.590909090909091,2.657142857142857),(0.0,0.0,3.0,0.8181818181818182),(3.0,0.0,5.590909090909091,0.8181818181818182),(0.0,0.0,3.0,0.0),(0.0,0.0,3.0,0.8181818181818182),(0.0,0.0,2.0,0.0),(2.0,0.0,3.0,0.0),(0.0,0.0,2.0,0.0),(0.0,0.0,2.0,0.0),(0.0,0.0,1.5,0.0),(1.5,0.0,2.0,0.0),(3.0,0.0,5.590909090909091,0.0),(3.0,0.0,5.590909090909091,0.8181818181818182),(3.0,0.0,4.5,0.0),(4.5,0.0,5.590909090909091,0.0),(0.0,0.8181818181818182,2.1666666666666665,2.657142857142857),(2.1666666666666665,0.8181818181818182,5.590909090909091,2.657142857142857),(0.0,0.8181818181818182,2.1666666666666665,1.5),(0.0,1.5,2.1666666666666665,2.657142857142857),(0.0,0.8181818181818182,0.5,1.5),(0.5,0.8181818181818182,2.1666666666666665,1.5),(0.0,1.5,1.5,2.657142857142857),(1.5,1.5,2.1666666666666665,2.657142857142857),(2.1666666666666665,0.8181818181818182,5.590909090909091,1.5),(2.1666666666666665,1.5,5.590909090909091,2.657142857142857),(5.590909090909091,0.0,11.0625,0.8181818181818182),(5.590909090909091,0.8181818181818182,11.0625,2.657142857142857),(5.590909090909091,0.0,8.6,0.8181818181818182),(8.6,0.0,11.0625,0.8181818181818182),(5.590909090909091,0.0,8.6,0.0),(5.590909090909091,0.0,8.6,0.8181818181818182),(5.590909090909091,0.0,6.5,0.0),(6.5,0.0,8.6,0.0),(8.6,0.0,11.0625,0.0),(8.6,0.0,11.0625,0.8181818181818182),(8.6,0.0,10.0,0.0),(10.0,0.0,11.0625,0.0),(8.6,0.0,10.0,0.0),(8.6,0.0,10.0,0.0),(8.6,0.0,9.5,0.0),(9.5,0.0,10.0,0.0),(5.590909090909091,0.8181818181818182,8.666666666666666,2.657142857142857),(8.666666666666666,0.8181818181818182,11.0625,2.657142857142857),(5.590909090909091,0.8181818181818182,8.666666666666666,1.5),(5.590909090909091,1.5,8.666666666666666,2.657142857142857),(8.666666666666666,0.8181818181818182,11.0625,1.5),(8.666666666666666,1.5,11.0625,2.657142857142857),(8.666666666666666,0.8181818181818182,9.5,1.5),(9.5,0.8181818181818182,11.0625,1.5),(8.666666666666666,1.5,10.0,2.657142857142857),(10.0,1.5,11.0625,2.657142857142857),(0.0,2.657142857142857,5.6923076923076925,13.0),(5.6923076923076925,2.657142857142857,11.0625,13.0),(0.0,2.657142857142857,5.6923076923076925,5.333333333333333),(0.0,5.333333333333333,5.6923076923076925,13.0),(0.0,2.657142857142857,1.75,5.333333333333333),(1.75,2.657142857142857,5.6923076923076925,5.333333333333333),(0.0,2.657142857142857,1.75,4.0),(0.0,4.0,1.75,5.333333333333333),(0.0,2.657142857142857,0.5,4.0),(0.5,2.657142857142857,1.75,4.0),(1.75,2.657142857142857,5.6923076923076925,4.5),(1.75,4.5,5.6923076923076925,5.333333333333333),(0.0,5.333333333333333,3.5,13.0),(3.5,5.333333333333333,5.6923076923076925,13.0),(5.6923076923076925,2.657142857142857,11.0625,6.142857142857143),(5.6923076923076925,6.142857142857143,11.0625,13.0),(5.6923076923076925,2.657142857142857,8.6,6.142857142857143),(8.6,2.657142857142857,11.0625,6.142857142857143),(5.6923076923076925,2.657142857142857,8.6,4.0),(5.6923076923076925,4.0,8.6,6.142857142857143),(5.6923076923076925,2.657142857142857,8.0,4.0),(8.0,2.657142857142857,8.6,4.0),(5.6923076923076925,2.657142857142857,8.0,3.5),(5.6923076923076925,3.5,8.0,4.0),(8.6,2.657142857142857,11.0625,5.0),(8.6,5.0,11.0625,6.142857142857143),(8.6,2.657142857142857,9.5,5.0),(9.5,2.657142857142857,11.0625,5.0),(5.6923076923076925,6.142857142857143,8.5,13.0),(8.5,6.142857142857143,11.0625,13.0),(11.0625,0.0,25.0,2.896551724137931),(11.0625,2.896551724137931,25.0,13.0),(11.0625,0.0,17.6875,2.896551724137931),(17.6875,0.0,25.0,2.896551724137931),(11.0625,0.0,17.6875,0.5714285714285714),(11.0625,0.5714285714285714,17.6875,2.896551724137931),(11.0625,0.0,14.5,0.5714285714285714),(14.5,0.0,17.6875,0.5714285714285714),(11.0625,0.0,14.5,0.0),(11.0625,0.0,14.5,0.5714285714285714),(11.0625,0.0,13.0,0.0),(13.0,0.0,14.5,0.0),(14.5,0.0,17.6875,0.0),(14.5,0.0,17.6875,0.5714285714285714),(14.5,0.0,16.0,0.0),(16.0,0.0,17.6875,0.0),(11.0625,0.5714285714285714,13.333333333333334,2.896551724137931),(13.333333333333334,0.5714285714285714,17.6875,2.896551724137931),(11.0625,0.5714285714285714,13.333333333333334,1.5),(11.0625,1.5,13.333333333333334,2.896551724137931),(17.6875,0.0,25.0,0.6666666666666666),(17.6875,0.6666666666666666,25.0,2.896551724137931),(17.6875,0.0,21.0,0.6666666666666666),(21.0,0.0,25.0,0.6666666666666666),(17.6875,0.0,21.0,0.0),(17.6875,0.0,21.0,0.6666666666666666),(17.6875,0.0,19.333333333333332,0.0),(19.333333333333332,0.0,21.0,0.0),(17.6875,0.0,19.333333333333332,0.0),(17.6875,0.0,19.333333333333332,0.0),(17.6875,0.0,18.5,0.0),(18.5,0.0,19.333333333333332,0.0),(21.0,0.0,25.0,0.0),(21.0,0.0,25.0,0.6666666666666666),(21.0,0.0,23.5,0.0),(23.5,0.0,25.0,0.0),(17.6875,0.6666666666666666,20.0,2.896551724137931),(20.0,0.6666666666666666,25.0,2.896551724137931),(17.6875,0.6666666666666666,20.0,1.5),(17.6875,1.5,20.0,2.896551724137931),(20.0,0.6666666666666666,25.0,1.5),(20.0,1.5,25.0,2.896551724137931),(11.0625,2.896551724137931,17.53846153846154,13.0),(17.53846153846154,2.896551724137931,25.0,13.0),(11.0625,2.896551724137931,17.53846153846154,6.166666666666667),(11.0625,6.166666666666667,17.53846153846154,13.0),(11.0625,2.896551724137931,14.75,6.166666666666667),(14.75,2.896551724137931,17.53846153846154,6.166666666666667),(14.75,2.896551724137931,17.53846153846154,4.333333333333333),(14.75,4.333333333333333,17.53846153846154,6.166666666666667),(14.75,4.333333333333333,16.0,6.166666666666667),(16.0,4.333333333333333,17.53846153846154,6.166666666666667),(11.0625,6.166666666666667,13.0,13.0),(13.0,6.166666666666667,17.53846153846154,13.0),(17.53846153846154,2.896551724137931,25.0,5.285714285714286),(17.53846153846154,5.285714285714286,25.0,13.0),(17.53846153846154,2.896551724137931,20.333333333333332,5.285714285714286),(20.333333333333332,2.896551724137931,25.0,5.285714285714286),(20.333333333333332,2.896551724137931,25.0,3.5),(20.333333333333332,3.5,25.0,5.285714285714286),(17.53846153846154,5.285714285714286,20.5,13.0),(20.5,5.285714285714286,25.0,13.0),(20.5,5.285714285714286,25.0,6.333333333333333),(20.5,6.333333333333333,25.0,13.0),(20.5,5.285714285714286,21.5,6.333333333333333),(21.5,5.285714285714286,25.0,6.333333333333333)]

gqu = [(11.0625,2.765625,25.0,13.0),(11.0625,0.0,25.0,2.765625),(0.0,0.0,11.0625,2.765625),(0.0,2.765625,11.0625,13.0),(17.53846153846154,5.6923076923076925,25.0,13.0),(17.53846153846154,2.765625,25.0,5.6923076923076925),(11.0625,2.765625,17.53846153846154,5.6923076923076925),(11.0625,5.6923076923076925,17.53846153846154,13.0),(20.5,6.25,25.0,13.0),(20.5,5.6923076923076925,25.0,6.25),(17.53846153846154,5.6923076923076925,20.5,6.25),(17.53846153846154,6.25,20.5,13.0),(21.5,6.0,25.0,6.25),(21.5,5.6923076923076925,25.0,6.0),(20.5,5.6923076923076925,21.5,6.0),(20.5,6.0,21.5,6.25),(20.333333333333332,4.0,25.0,5.6923076923076925),(20.333333333333332,2.765625,25.0,4.0),(17.53846153846154,2.765625,20.333333333333332,4.0),(17.53846153846154,4.0,20.333333333333332,5.6923076923076925),(14.75,4.25,17.53846153846154,5.6923076923076925),(14.75,2.765625,17.53846153846154,4.25),(11.0625,2.765625,14.75,4.25),(11.0625,4.25,14.75,5.6923076923076925),(16.0,5.0,17.53846153846154,5.6923076923076925),(16.0,4.25,17.53846153846154,5.0),(14.75,4.25,16.0,5.0),(14.75,5.0,16.0,5.6923076923076925),(13.0,10.0,17.53846153846154,13.0),(13.0,5.6923076923076925,17.53846153846154,10.0),(11.0625,5.6923076923076925,13.0,10.0),(11.0625,10.0,13.0,13.0),(17.6875,0.625,25.0,2.765625),(17.6875,0.0,25.0,0.625),(11.0625,0.0,17.6875,0.625),(11.0625,0.625,17.6875,2.765625),(20.0,1.5,25.0,2.765625),(20.0,0.625,25.0,1.5),(17.6875,0.625,20.0,1.5),(17.6875,1.5,20.0,2.765625),(21.0,0.0,25.0,0.625),(21.0,0.0,25.0,0.0),(17.6875,0.0,21.0,0.0),(17.6875,0.0,21.0,0.625),(22.666666666666668,0.0,25.0,0.625),(22.666666666666668,0.0,25.0,0.0),(21.0,0.0,22.666666666666668,0.0),(21.0,0.0,22.666666666666668,0.625),(21.5,0.0,22.666666666666668,0.625),(21.5,0.0,22.666666666666668,0.0),(21.0,0.0,21.5,0.0),(21.0,0.0,21.5,0.625),(18.5,0.0,21.0,0.625),(18.5,0.0,21.0,0.0),(17.6875,0.0,18.5,0.0),(17.6875,0.0,18.5,0.625),(14.5,0.0,17.6875,0.625),(14.5,0.0,17.6875,0.0),(11.0625,0.0,14.5,0.0),(11.0625,0.0,14.5,0.625),(16.0,0.0,17.6875,0.625),(16.0,0.0,17.6875,0.0),(14.5,0.0,16.0,0.0),(14.5,0.0,16.0,0.625),(13.0,0.0,14.5,0.625),(13.0,0.0,14.5,0.0),(11.0625,0.0,13.0,0.0),(11.0625,0.0,13.0,0.625),(13.333333333333334,1.3333333333333333,17.6875,2.765625),(13.333333333333334,0.625,17.6875,1.3333333333333333),(11.0625,0.625,13.333333333333334,1.3333333333333333),(11.0625,1.3333333333333333,13.333333333333334,2.765625),(5.590909090909091,0.8181818181818182,11.0625,2.765625),(5.590909090909091,0.0,11.0625,0.8181818181818182),(0.0,0.0,5.590909090909091,0.8181818181818182),(0.0,0.8181818181818182,5.590909090909091,2.765625),(8.666666666666666,1.5,11.0625,2.765625),(8.666666666666666,0.8181818181818182,11.0625,1.5),(5.590909090909091,0.8181818181818182,8.666666666666666,1.5),(5.590909090909091,1.5,8.666666666666666,2.765625),(10.0,2.0,11.0625,2.765625),(10.0,1.5,11.0625,2.0),(8.666666666666666,1.5,10.0,2.0),(8.666666666666666,2.0,10.0,2.765625),(9.5,1.0,11.0625,1.5),(9.5,0.8181818181818182,11.0625,1.0),(8.666666666666666,0.8181818181818182,9.5,1.0),(8.666666666666666,1.0,9.5,1.5),(8.6,0.0,11.0625,0.8181818181818182),(8.6,0.0,11.0625,0.0),(5.590909090909091,0.0,8.6,0.0),(5.590909090909091,0.0,8.6,0.8181818181818182),(10.0,0.0,11.0625,0.8181818181818182),(10.0,0.0,11.0625,0.0),(8.6,0.0,10.0,0.0),(8.6,0.0,10.0,0.8181818181818182),(10.5,0.0,11.0625,0.8181818181818182),(10.5,0.0,11.0625,0.0),(10.0,0.0,10.5,0.0),(10.0,0.0,10.5,0.8181818181818182),(6.5,0.0,8.6,0.8181818181818182),(6.5,0.0,8.6,0.0),(5.590909090909091,0.0,6.5,0.0),(5.590909090909091,0.0,6.5,0.8181818181818182),(3.0,0.0,5.590909090909091,0.8181818181818182),(3.0,0.0,5.590909090909091,0.0),(0.0,0.0,3.0,0.0),(0.0,0.0,3.0,0.8181818181818182),(4.0,0.0,5.590909090909091,0.8181818181818182),(4.0,0.0,5.590909090909091,0.0),(3.0,0.0,4.0,0.0),(3.0,0.0,4.0,0.8181818181818182),(4.5,0.0,5.590909090909091,0.8181818181818182),(4.5,0.0,5.590909090909091,0.0),(4.0,0.0,4.5,0.0),(4.0,0.0,4.5,0.8181818181818182),(1.5,0.0,3.0,0.8181818181818182),(1.5,0.0,3.0,0.0),(0.0,0.0,1.5,0.0),(0.0,0.0,1.5,0.8181818181818182),(2.1666666666666665,1.5,5.590909090909091,2.765625),(2.1666666666666665,0.8181818181818182,5.590909090909091,1.5),(0.0,0.8181818181818182,2.1666666666666665,1.5),(0.0,1.5,2.1666666666666665,2.765625),(0.5,1.0,2.1666666666666665,1.5),(0.5,0.8181818181818182,2.1666666666666665,1.0),(0.0,0.8181818181818182,0.5,1.0),(0.0,1.0,0.5,1.5),(1.5,2.0,2.1666666666666665,2.765625),(1.5,1.5,2.1666666666666665,2.0),(0.0,1.5,1.5,2.0),(0.0,2.0,1.5,2.765625),(5.6923076923076925,5.769230769230769,11.0625,13.0),(5.6923076923076925,2.765625,11.0625,5.769230769230769),(0.0,2.765625,5.6923076923076925,5.769230769230769),(0.0,5.769230769230769,5.6923076923076925,13.0),(8.5,10.5,11.0625,13.0),(8.5,5.769230769230769,11.0625,10.5),(5.6923076923076925,5.769230769230769,8.5,10.5),(5.6923076923076925,10.5,8.5,13.0),(8.6,4.4,11.0625,5.769230769230769),(8.6,2.765625,11.0625,4.4),(5.6923076923076925,2.765625,8.6,4.4),(5.6923076923076925,4.4,8.6,5.769230769230769),(9.5,5.0,11.0625,5.769230769230769),(9.5,4.4,11.0625,5.0),(8.6,4.4,9.5,5.0),(8.6,5.0,9.5,5.769230769230769),(8.0,3.5,8.6,4.4),(8.0,2.765625,8.6,3.5),(5.6923076923076925,2.765625,8.0,3.5),(5.6923076923076925,3.5,8.0,4.4),(1.75,4.25,5.6923076923076925,5.769230769230769),(1.75,2.765625,5.6923076923076925,4.25),(0.0,2.765625,1.75,4.25),(0.0,4.25,1.75,5.769230769230769),(0.5,4.0,1.75,4.25),(0.5,2.765625,1.75,4.0),(0.0,2.765625,0.5,4.0),(0.0,4.0,0.5,4.25),(3.5,7.5,5.6923076923076925,13.0),(3.5,5.769230769230769,5.6923076923076925,7.5),(0.0,5.769230769230769,3.5,7.5),(0.0,7.5,3.5,13.0)]

rat = [(0.0,0.0,11.0625,13.0),(11.0625,0.0,25.0,13.0),(0.0,0.0,5.628571428571429,13.0),(5.628571428571429,0.0,11.0625,13.0),(0.0,0.0,2.4705882352941178,13.0),(2.4705882352941178,0.0,5.628571428571429,13.0),(0.0,0.0,1.1111111111111112,13.0),(1.1111111111111112,0.0,2.4705882352941178,13.0),(0.0,0.0,0.6666666666666666,13.0),(0.6666666666666666,0.0,1.1111111111111112,13.0),(0.0,0.0,1.1111111111111112,2.0),(0.0,2.0,1.1111111111111112,13.0),(0.0,0.0,1.1111111111111112,1.0),(0.0,1.0,1.1111111111111112,2.0),(0.0,0.0,1.1111111111111112,0.6666666666666666),(0.0,0.6666666666666666,1.1111111111111112,1.0),(0.0,0.0,2.4705882352941178,2.0),(0.0,2.0,2.4705882352941178,13.0),(0.0,0.0,2.4705882352941178,1.0),(0.0,1.0,2.4705882352941178,2.0),(0.0,0.0,2.4705882352941178,0.5),(0.0,0.5,2.4705882352941178,1.0),(2.4705882352941178,0.0,4.0,13.0),(4.0,0.0,5.628571428571429,13.0),(2.4705882352941178,0.0,3.6666666666666665,13.0),(3.6666666666666665,0.0,4.0,13.0),(2.4705882352941178,0.0,4.0,3.5),(2.4705882352941178,3.5,4.0,13.0),(2.4705882352941178,0.0,4.0,0.3333333333333333),(2.4705882352941178,0.3333333333333333,4.0,3.5),(2.4705882352941178,3.5,4.0,6.666666666666667),(2.4705882352941178,6.666666666666667,4.0,13.0),(2.4705882352941178,6.666666666666667,4.0,7.5),(2.4705882352941178,7.5,4.0,13.0),(2.4705882352941178,0.0,5.628571428571429,2.875),(2.4705882352941178,2.875,5.628571428571429,13.0),(2.4705882352941178,0.0,5.628571428571429,0.6),(2.4705882352941178,0.6,5.628571428571429,2.875),(2.4705882352941178,0.6,5.628571428571429,1.5),(2.4705882352941178,1.5,5.628571428571429,2.875),(2.4705882352941178,2.875,5.628571428571429,6.666666666666667),(2.4705882352941178,6.666666666666667,5.628571428571429,13.0),(2.4705882352941178,6.666666666666667,5.628571428571429,7.5),(2.4705882352941178,7.5,5.628571428571429,13.0),(0.0,0.0,5.628571428571429,2.411764705882353),(0.0,2.411764705882353,5.628571428571429,13.0),(0.0,0.0,5.628571428571429,0.8181818181818182),(0.0,0.8181818181818182,5.628571428571429,2.411764705882353),(0.0,0.8181818181818182,5.628571428571429,1.5),(0.0,1.5,5.628571428571429,2.411764705882353),(0.0,2.411764705882353,5.628571428571429,5.333333333333333),(0.0,5.333333333333333,5.628571428571429,13.0),(0.0,2.411764705882353,5.628571428571429,4.25),(0.0,4.25,5.628571428571429,5.333333333333333),(0.0,5.333333333333333,5.628571428571429,7.5),(0.0,7.5,5.628571428571429,13.0),(5.628571428571429,0.0,8.61111111111111,13.0),(8.61111111111111,0.0,11.0625,13.0),(5.628571428571429,0.0,7.125,13.0),(7.125,0.0,8.61111111111111,13.0),(5.628571428571429,0.0,6.6,13.0),(6.6,0.0,7.125,13.0),(5.628571428571429,0.0,7.125,2.2),(5.628571428571429,2.2,7.125,13.0),(5.628571428571429,0.0,7.125,0.75),(5.628571428571429,0.75,7.125,2.2),(5.628571428571429,0.75,7.125,1.5),(5.628571428571429,1.5,7.125,2.2),(5.628571428571429,0.0,8.61111111111111,2.875),(5.628571428571429,2.875,8.61111111111111,13.0),(5.628571428571429,0.0,8.61111111111111,0.75),(5.628571428571429,0.75,8.61111111111111,2.875),(5.628571428571429,0.75,8.61111111111111,1.5),(5.628571428571429,1.5,8.61111111111111,2.875),(5.628571428571429,2.875,8.61111111111111,5.0),(5.628571428571429,5.0,8.61111111111111,13.0),(5.628571428571429,2.875,8.61111111111111,4.0),(5.628571428571429,4.0,8.61111111111111,5.0),(5.628571428571429,2.875,8.61111111111111,3.5),(5.628571428571429,3.5,8.61111111111111,4.0),(8.61111111111111,0.0,9.8,13.0),(9.8,0.0,11.0625,13.0),(9.8,0.0,10.333333333333334,13.0),(10.333333333333334,0.0,11.0625,13.0),(9.8,0.0,11.0625,3.5),(9.8,3.5,11.0625,13.0),(9.8,0.0,11.0625,0.75),(9.8,0.75,11.0625,3.5),(9.8,0.75,11.0625,1.5),(9.8,1.5,11.0625,3.5),(9.8,3.5,11.0625,9.0),(9.8,9.0,11.0625,13.0),(8.61111111111111,0.0,11.0625,2.9),(8.61111111111111,2.9,11.0625,13.0),(8.61111111111111,0.0,11.0625,0.8571428571428571),(8.61111111111111,0.8571428571428571,11.0625,2.9),(8.61111111111111,0.8571428571428571,11.0625,1.5),(8.61111111111111,1.5,11.0625,2.9),(8.61111111111111,2.9,11.0625,7.666666666666667),(8.61111111111111,7.666666666666667,11.0625,13.0),(5.628571428571429,0.0,11.0625,2.888888888888889),(5.628571428571429,2.888888888888889,11.0625,13.0),(5.628571428571429,0.0,11.0625,0.8181818181818182),(5.628571428571429,0.8181818181818182,11.0625,2.888888888888889),(5.628571428571429,0.8181818181818182,11.0625,1.5),(5.628571428571429,1.5,11.0625,2.888888888888889),(5.628571428571429,2.888888888888889,11.0625,6.142857142857143),(5.628571428571429,6.142857142857143,11.0625,13.0),(5.628571428571429,2.888888888888889,11.0625,4.4),(5.628571428571429,4.4,11.0625,6.142857142857143),(5.628571428571429,2.888888888888889,11.0625,3.5),(5.628571428571429,3.5,11.0625,4.4),(5.628571428571429,6.142857142857143,11.0625,10.5),(5.628571428571429,10.5,11.0625,13.0),(0.0,0.0,11.0625,2.657142857142857),(0.0,2.657142857142857,11.0625,13.0),(0.0,0.0,11.0625,0.8181818181818182),(0.0,0.8181818181818182,11.0625,2.657142857142857),(0.0,0.8181818181818182,11.0625,1.5),(0.0,1.5,11.0625,2.657142857142857),(0.0,2.657142857142857,11.0625,5.769230769230769),(0.0,5.769230769230769,11.0625,13.0),(0.0,2.657142857142857,11.0625,4.333333333333333),(0.0,4.333333333333333,11.0625,5.769230769230769),(0.0,2.657142857142857,11.0625,3.8),(0.0,3.8,11.0625,4.333333333333333),(0.0,5.769230769230769,11.0625,9.0),(0.0,9.0,11.0625,13.0),(0.0,5.769230769230769,11.0625,7.666666666666667),(0.0,7.666666666666667,11.0625,9.0),(11.0625,0.0,17.620689655172413,13.0),(17.620689655172413,0.0,25.0,13.0),(11.0625,0.0,14.076923076923077,13.0),(14.076923076923077,0.0,17.620689655172413,13.0),(11.0625,0.0,12.714285714285714,13.0),(12.714285714285714,0.0,14.076923076923077,13.0),(12.714285714285714,0.0,13.666666666666666,13.0),(13.666666666666666,0.0,14.076923076923077,13.0),(12.714285714285714,0.0,14.076923076923077,3.6666666666666665),(12.714285714285714,3.6666666666666665,14.076923076923077,13.0),(12.714285714285714,0.0,14.076923076923077,1.0),(12.714285714285714,1.0,14.076923076923077,3.6666666666666665),(11.0625,0.0,14.076923076923077,3.857142857142857),(11.0625,3.857142857142857,14.076923076923077,13.0),(11.0625,0.0,14.076923076923077,0.75),(11.0625,0.75,14.076923076923077,3.857142857142857),(11.0625,0.75,14.076923076923077,1.5),(11.0625,1.5,14.076923076923077,3.857142857142857),(11.0625,3.857142857142857,14.076923076923077,8.0),(11.0625,8.0,14.076923076923077,13.0),(11.0625,8.0,14.076923076923077,10.0),(11.0625,10.0,14.076923076923077,13.0),(14.076923076923077,0.0,15.666666666666666,13.0),(15.666666666666666,0.0,17.620689655172413,13.0),(14.076923076923077,0.0,17.620689655172413,2.3333333333333335),(14.076923076923077,2.3333333333333335,17.620689655172413,13.0),(14.076923076923077,0.0,17.620689655172413,0.3333333333333333),(14.076923076923077,0.3333333333333333,17.620689655172413,2.3333333333333335),(14.076923076923077,2.3333333333333335,17.620689655172413,4.333333333333333),(14.076923076923077,4.333333333333333,17.620689655172413,13.0),(11.0625,0.0,17.620689655172413,3.1538461538461537),(11.0625,3.1538461538461537,17.620689655172413,13.0),(11.0625,0.0,17.620689655172413,0.875),(11.0625,0.875,17.620689655172413,3.1538461538461537),(11.0625,0.875,17.620689655172413,1.75),(11.0625,1.75,17.620689655172413,3.1538461538461537),(11.0625,1.75,17.620689655172413,2.5),(11.0625,2.5,17.620689655172413,3.1538461538461537),(11.0625,3.1538461538461537,17.620689655172413,6.8),(11.0625,6.8,17.620689655172413,13.0),(11.0625,3.1538461538461537,17.620689655172413,4.666666666666667),(11.0625,4.666666666666667,17.620689655172413,6.8),(11.0625,6.8,17.620689655172413,10.0),(11.0625,10.0,17.620689655172413,13.0),(17.620689655172413,0.0,20.5,13.0),(20.5,0.0,25.0,13.0),(17.620689655172413,0.0,18.166666666666668,13.0),(18.166666666666668,0.0,20.5,13.0),(17.620689655172413,0.0,20.5,2.3333333333333335),(17.620689655172413,2.3333333333333335,20.5,13.0),(17.620689655172413,0.0,20.5,0.75),(17.620689655172413,0.75,20.5,2.3333333333333335),(17.620689655172413,0.75,20.5,1.5),(17.620689655172413,1.5,20.5,2.3333333333333335),(17.620689655172413,2.3333333333333335,20.5,5.5),(17.620689655172413,5.5,20.5,13.0),(20.5,0.0,21.9,13.0),(21.9,0.0,25.0,13.0),(21.9,0.0,22.5,13.0),(22.5,0.0,25.0,13.0),(21.9,0.0,25.0,2.0),(21.9,2.0,25.0,13.0),(21.9,0.0,25.0,0.75),(21.9,0.75,25.0,2.0),(21.9,0.75,25.0,1.5),(21.9,1.5,25.0,2.0),(21.9,2.0,25.0,4.5),(21.9,4.5,25.0,13.0),(20.5,0.0,25.0,2.9),(20.5,2.9,25.0,13.0),(20.5,0.0,25.0,0.6),(20.5,0.6,25.0,2.9),(20.5,0.6,25.0,1.5),(20.5,1.5,25.0,2.9),(20.5,2.9,25.0,5.2),(20.5,5.2,25.0,13.0),(20.5,2.9,25.0,3.5),(20.5,3.5,25.0,5.2),(20.5,5.2,25.0,6.333333333333333),(20.5,6.333333333333333,25.0,13.0),(17.620689655172413,0.0,25.0,2.6875),(17.620689655172413,2.6875,25.0,13.0),(17.620689655172413,0.0,25.0,0.6666666666666666),(17.620689655172413,0.6666666666666666,25.0,2.6875),(17.620689655172413,0.6666666666666666,25.0,1.5),(17.620689655172413,1.5,25.0,2.6875),(17.620689655172413,2.6875,25.0,5.285714285714286),(17.620689655172413,5.285714285714286,25.0,13.0),(17.620689655172413,2.6875,25.0,4.0),(17.620689655172413,4.0,25.0,5.285714285714286),(17.620689655172413,2.6875,25.0,3.5),(17.620689655172413,3.5,25.0,4.0),(17.620689655172413,5.285714285714286,25.0,6.25),(17.620689655172413,6.25,25.0,13.0),(11.0625,0.0,25.0,2.896551724137931),(11.0625,2.896551724137931,25.0,13.0),(11.0625,0.0,25.0,0.625),(11.0625,0.625,25.0,2.896551724137931),(11.0625,0.625,25.0,1.4285714285714286),(11.0625,1.4285714285714286,25.0,2.896551724137931),(11.0625,2.896551724137931,25.0,5.6923076923076925),(11.0625,5.6923076923076925,25.0,13.0),(11.0625,2.896551724137931,25.0,4.142857142857143),(11.0625,4.142857142857143,25.0,5.6923076923076925),(11.0625,2.896551724137931,25.0,3.5),(11.0625,3.5,25.0,4.142857142857143),(11.0625,5.6923076923076925,25.0,7.5),(11.0625,7.5,25.0,13.0),(11.0625,5.6923076923076925,25.0,6.25),(11.0625,6.25,25.0,7.5),(11.0625,7.5,25.0,10.0),(11.0625,10.0,25.0,13.0),(0.0,0.0,25.0,2.765625),(0.0,2.765625,25.0,13.0),(0.0,0.0,25.0,0.7368421052631579),(0.0,0.7368421052631579,25.0,2.765625),(0.0,0.7368421052631579,25.0,1.4736842105263157),(0.0,1.4736842105263157,25.0,2.765625),(0.0,2.765625,25.0,5.730769230769231),(0.0,5.730769230769231,25.0,13.0),(0.0,2.765625,25.0,4.25),(0.0,4.25,25.0,5.730769230769231),(0.0,2.765625,25.0,3.6666666666666665),(0.0,3.6666666666666665,25.0,4.25),(0.0,5.730769230769231,25.0,8.1),(0.0,8.1,25.0,13.0),(0.0,5.730769230769231,25.0,6.857142857142857),(0.0,6.857142857142857,25.0,8.1),(0.0,6.857142857142857,25.0,7.5),(0.0,7.5,25.0,8.1),(0.0,8.1,25.0,11.0),(0.0,11.0,25.0,13.0),(0.0,8.1,25.0,10.0),(0.0,10.0,25.0,11.0)]
