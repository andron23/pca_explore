import pickle
import numpy as np
from pathlib import Path 
import glob

from sklearn.decomposition import PCA
from scipy.spatial.distance import pdist
from collections import OrderedDict

path_gen_facebank_embs = Path('gen_facebank_embs/')

with open('Voloshin_basmat.pickle', 'rb') as f: x_old = pickle.load(f) 
x_old = np.asarray(x_old)


with open(str(path_gen_facebank_embs)+'/Voloshin_embeddings.pickle', 'rb') as f: emb = pickle.load(f) 
emb = np.asarray(list(emb.values()))[0]




from sklearn import decomposition
from sklearn import datasets

centers = [[1, 1], [-1, -1], [1, -1]]
iris = datasets.load_iris()


#Форма X (количество измерений, количество признаков)
X = iris.data
y = iris.target



#Считаем ковариационную матрицу размерности (кол-во признаков, кол-во признаков)
covmat = np.cov(X.transpose())    

#Производим спектральное разложение. i столбец-айгенвектор vecs[:,i] соответствует i собственному числу. Длина айгенвектора - количество исходных признаков. 
#По строкам - количество всех изначальных признаков. 
values, vecs = np.linalg.eig(covmat)

#Выбираем N главных компонент и транспонируем, теперь компоненты идут по строкам. По столбцам - номер компонента. 
v = vecs[:,0:3].transpose()


pca = decomposition.PCA(n_components=6)
pca.fit(X)


#N главных компонент расположены по строкам. По столбцам - номер компонента (т.е. исходного признака). 
pca.components_

#Проекция исходных признаков на матрицу главных компонент. Исходные данные (кол-во измерений, кол-во признаков)*ГК(кол-во признаков, число ГК). 
#В таблице значения измерения в новых переменных (т.е. координаты по новым признакам). 
#Формула: np.dot(X[0]-pca.mean_, pca.components_.transpose()) 
XX = pca.transform(X)


def PCA(data, n_components):
    pca = decomposition.PCA(n_components=n_components)
    pca.fit(data)
    components = pca.components_
    mean = pca.mean_
    return(components, mean)

def projection(vec, components, mean):
    projection = np.dot(vec-mean, components.transpose()) 
    return(projection)

def open_embs(name):
    path_gen_facebank_embs = Path('gen_facebank_embs/')
    with open(str(path_gen_facebank_embs) + '/{}_embeddings.pickle'.format(name), 'rb') as f: 
        emb = pickle.load(f)
    emb = np.array(list(emb.values()))
    return(emb)