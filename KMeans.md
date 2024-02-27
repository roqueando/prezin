## Explicação KMeans

O algoritmo K-Means serve para criar uma `clusterização` dos dados e distingui-los em `clusters` diferentes.
Cada ponto num plot é considerado um `data point`, e os mesmos não possuem `labels`.

>#### Tradução de algumas palavras

- Clusterização -> Agrupamento
- Clusters -> grupos
- Data Point -> Ponto de Dados
    - Esses pontos de dados são numeros que são extraidos depois de pre processar algum dado especifico, seja uma imagem ou registros de um banco de dados.
- Labels -> Rotulos
    - Esses rotulos são usados em algoritmos de classificação na qual é necessário ter um "alvo" para checar o quanto o algoritmo acertou.
- Plot -> Grafico ou plano
    - Normalmente usamos os plots para visualizar os dados que temos depois de serem pré processados

### Passo a passo

0. Antes de tudo precisamos definir um K, e essa constante será o número de clusters que queremos ter. Existem métodos como Elbow Method e Cross Validation para achar o melhor K e criar clusters consistentes.
1. Primeiro inicializamos K centróides em um lugares aleatórios no plot.
2. Segundo calculamos a distância entre cada data point e K centróides.
3. Depois que é calculada a distância, a menor entre cada centróide se torna um cluster, e então reposicionamos esses centróides na posição média entre esses data points

A partir desses passos, a iteração segue entre o passo 2 e 3, pois depois de reposicionados os centróides, é necessário recalcular a distância para reavaliar os clusters

- Pode também ser definido uma quantidade de iteração para que isso ocorra
- Ou caso os centróides parem de se mover


```python
y = [1, 0, 2, 0, 1 ,2]
```

a função argwhere do numpy retorna os indices de elementos não-zero de acordo com uma condicional ou não.

utilizando como elxemplo esse `y` acima:
```python
numpy.argwhere(y > 1)
[
    [0, 2],
    [0, 5]
]
```
