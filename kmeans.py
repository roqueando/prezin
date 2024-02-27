import numpy as np
import matplotlib.pyplot as plt

class KMeansClustering:
    def __init__(self, k=3):
        self.k = k
        self.centroids = None

    @staticmethod
    def euclidian_distance(data_point, centroids):
        return np.sqrt(np.sum((centroids - data_point)**2, axis=1))

    def fit(self, X, max_iterations=200):
       y = []
        # cria os centroides de maneira aleatória para um número de K
       self.centroids = np.random.uniform(np.amin(X, axis=0), np.amax(X, axis=0),
           size=(self.k, X.shape[1]))

        # itera sobre o max_iterations
       for _ in range(max_iterations):
           # Cria um target próprio, esse algoritmo meio que produz seu próprio Y
           y = []

           # para cada datapoint no conjunto X
           for data_point in X:
               # calcule a distância euclidiana entre o datapoint
               # (vamos chamar de DPi) e os centroides
               distances = KMeansClustering.euclidian_distance(data_point=data_point, centroids=self.centroids)

               # argmin pega o menor numero e seu index nesse array `distance`
               cluster_number = np.argmin(distances)

               # coloca o index dentro do y
               y.append(cluster_number)

           y = np.array(y)

           cluster_indexes = []
           for i in range(self.k):
               cluster_indexes.append(np.argwhere(y == i))

           cluster_centers = []
           for i, indices in enumerate(cluster_indexes):
               if len(indices) == 0:
                   cluster_centers.append(self.centroids[i])
               else:
                   cluster_centers.append(np.mean(X[indices], axis=0)[0])

           if np.max(self.centroids - np.array(cluster_centers)) < 0.0001:
               break
           else:
               self.centroids = np.array(cluster_centers)

       return y


new_X = [
    [
        1.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0
    ],
    [
        0.0,
        1.0,
        0.0,
        0.0,
        0.0,
        0.0
    ],
    [
        0.0,
        0.0,
        1.0,
        0.0,
        0.0,
        0.0
    ],
    [
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        0.0
    ],
    [
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0
    ],
    [
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0
    ]
]
X = np.array(new_X)
kmeans = KMeansClustering()
labels = kmeans.fit(X=X)
# plt.scatter(X[:, 0], X[:, 1], c=labels)
# plt.scatter(kmeans.centroids[:, 0], kmeans.centroids[:, 1], c=range(len(kmeans.centroids)), marker="*", s=200)

random_points = np.random.randint(0, 100, (100,2))
new_kmeans = KMeansClustering()
new_labels = new_kmeans.fit(X=random_points)
plt.scatter(random_points[:, 0], random_points[:, 1], c=new_labels)
plt.scatter(new_kmeans.centroids[:, 0], new_kmeans.centroids[:, 1], c=range(len(new_kmeans.centroids)), marker="*", s=200)

plt.show()
