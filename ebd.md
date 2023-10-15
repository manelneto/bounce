# EBD: Componente de Especificação da Base de Dados

O ***Community Connect*** é um sistema de informação web-based que permite aos utilizadores partilharem perguntas e obterem respostas sobre diversos temas, tendo como intuito fornecer respostas para problemas comuns, num ambiente de interajuda solidária.

## A4: Modelo Conceptual de Dados

O modelo conceptual de dados, constituído pelo diagrama UML de classes e pelas regras de negócio, procura elencar e caracterizar os elementos significativos do sistema (classes) e a forma como eles se relacionam entre si (associações). Este modelo é o ponto de partida para o esquema relacional da base de dados.


### 1. Diagrama de Classes

O diagrama de classes do ***Community Connect*** encontra-se representado na Figura 1. Este diagrama inclui todas as entidades (classes) relevantes para a especificação da base de dados, as associações entre elas e a respetiva multiplicidade, bem como os seus atributos e os domínios/restrições correspondentes.

![Diagrama de Classes](uploads/EBD-A4-DiagramaClasses.png)

Figura 1 - Diagrama de Classes


### 2. Regras de Negócio Adicionais
 
As restrições e regras de negócio que não puderam ser incluídas no diagrama de classes elencam-se na Tabela 1.

| Identificador | Descrição |
| ------------- | --------- |
| BR01 | Nenhum utilizador autenticado (*Authenticated*) pode votar (*Vote*) numa publicação (*Post*) da qual é autor
| BR02 | Nenhum utilizador autenticado (*Authenticated*) pode publicar uma resposta (*Answer*) numa pergunta (*Question*) da qual é autor
| BR03 | Nenhum utilizador autenticado (*Authenticated*) pode publicar um comentário (*Comment*) numa publicação (*Post*) da qual é autor
| BR04 | Se um utilizador autenticado (*Authenticated*) for apagado, o conteúdo do qual é autor mantém-se na base de dados com um autor anónimo |
| BR05 | O *rating* de um utilizador autenticado (*Authenticated*) numa comunidade (*Community*) é calculado de acordo com a fórmula 1000 x likes/(likes + dislikes) nas suas respostas (*Answer*) dentro dessa comunidade |
| BR06 | Um utilizador autenticado (*Authenticated*) é *expert* de uma comunidade (*Community*) se e só se tiver todos os emblemas (*Badge*) possíveis e um *rating* superior a 800 |
| BR07 | Os ficheiros (*file*) das publicações (*Post*) devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* |
| BR08 | Cada utilizador autenticado (*Authenticated*) só pode votar (*Vote*) em cada publicação (*Post*) uma vez

Tabela 1 - Regras de Negócio Adicionais


---


## A5: Esquema Relacional, Validação e Afinação

O esquema relacional, convertido a partir do modelo conceptual, mapeia as classes e associações em relações/tabelas, bem como os respetivos atributos e restrições. Quando devidamente validado e afinado, o esquema relacional garante a minimização/eliminação de anomalias, quer de redundância, quer de atualização/eliminação.


### 1. Esquema Relacional

O esquema relacional do ***Community Connect*** encontra-se, em notação compacta, na Tabela 2. Este inclui os esquemas das relações (tabelas), bem como os seus atributos e respetivas restrições (domínios, atributos chave primária, atributos chave externa e restrições de intregridade: NOT NULL, UNIQUE, DEFAULT e CHECK).  

| Referência da Relação | Notação Compacta da Relação |
| --------------------- | --------------------------- |
| R01 | user(<ins>id</ins>, username **NN** **UK**, email **NN** **UK**, password **NN**, register_date **NN** **CK** register_date <= Today, administrator **NN** **DF** false, blocked **NN** **DF** false, image) |
| R02 | notification(<ins>id</ins>, content **NN**, date **NN** **CK** date <= Today, read **NN** **DF** false, id_user → user **NN**) |
| R03 | badge(<ins>id</ins>, name **NN** **UK**) |
| R04 | user_earns_badge(<ins>id_user → user</ins>, <ins>id_badge → badge</ins>) |
| R05 | tag(<ins>id</ins>, name **NN** **UK**) |
| R06 | user_follows_tag(<ins>id_user → user</ins>, <ins>id_tag → tag</ins>) |
| R07 | community(<ins>id</ins>, name **NN** **UK**) |
| R08 | user_follows_community(<ins>id_user → user</ins>, <ins>id_community → community</ins>) |
| R09 | user_moderates_community(<ins>id_user → user</ins>, <ins>id_community → community</ins>) |
| R10 | reputation(<ins>id_user → user</ins>, <ins>id_community → community</ins>, rating **NN** **DF** 0 **CK** rating >= 0, expert **NN** **DF** false) |
| R11 | question(<ins>id</ins>, content **NN**, date **NN** **CK** date <= Today, file, last_edited **CK** last_edited <= Today AND last_edited >= date, title **NN**, id_user → user **NN**, id_community → community **NN**) |
| R12 | user_follows_question(<ins>id_user → user</ins>, <ins>id_question → question</ins>) |
| R13 | question_tags(<ins>id_question → question</ins>, <ins>id_tag → tag</ins>) |
| R14 | question_vote(<ins>id_question → question</ins>, <ins>id_user → user</ins>, like **NN**) |
| R15 | question_comment(<ins>id</ins>, content **NN**, date **NN** **CK** date <= Today, last_edited **CK** last_edited <= Today AND last_edited >= date, id_user → user **NN**, id_question → question **NN**) |
| R16 | answer(<ins>id</ins>, content **NN**, date **NN** **CK** date <= Today, file, last_edited **CK** last_edited <= Today AND last_edited >= date, correct **NN** **DF** false, id_user → user **NN**, id_question → question **NN**) |
| R17 | answer_vote(<ins>id_answer → answer</ins>, <ins>id_user → user</ins>, like **NN**) |
| R18 | answer_comment(<ins>id</ins>, content **NN**, date **NN** **CK** date <= Today, last_edited **CK** last_edited <= Today AND last_edited >= date, id_user → user **NN**, id_answer → answer **NN**) |

Tabela 2 - Esquema Relacional

Legenda:
- NN = NOT NULL
- UK = UNIQUE
- DF = DEFAULT
- CK = CHECK

A generalização das classes *Question* e *Answer* em *Post*, isto é, a especialização da classe *Post* em *Question* e *Answer*, foi convertida do modelo conceptual para o esquema relacional segundo uma abordagem orientada a objetos. De acordo com Jeffrey Ullman e Jennifer Widom em *A first course in Database Systems 3<sup>rd</sup> Edition* (Secção 4.8.2) e com https://www.cs.uct.ac.za/mit_notes/database/htmls/chp07.html, este método é o que mais se adequada a generalizações disjuntas e completas - como é o caso. Tal deve-se, principalmente, ao facto de, sendo a generalização disjunta e completa, o número de relações a serem criadas ser exatamente o número de subclasses, dado que nenhum elemento pode pertencer às duas subclasses em simultâneo (a generalização não seria disjunta) ou apenas à superclasse (a generalização não seria completa). Esta abordagem permite minimizar o espaço ocupado (evita-se a tabela para a superclasse) e facilita futuras consultas à base de dados, minimizando o número de junções necessárias para obter informação, em comparação com uma abordagem de mapeamento Entidade-Relação.


### 2. Domínios


Na Tabela 3, especifica-se o único domínio adicional.

| Nome do Domínio | Especificação do Domínio |
| ----------- | ---------------------------- |
| Today	| DATE DEFAULT CURRENT_DATE |

Tabela 3 - Domínio Adicional


### 3. Validação do Esquema


A validação do esquema relacional é essencial para minimizar/eliminar anomalias de redundância e de atualização/eliminação. Nesse sentido, nas tabelas abaixo apresentam-se as dependências funcionais de todas as relações e a respetiva relação normalizada (na verdade, em todos os casos, a própria relação já está normalizada).

| **TABELA R01** | user |
| -------------- | ---- |
| **Chaves** | {id}, {username}, {email}  |
| **Dependências Funcionais** |
| FD0101 | {id} → {username, email, password, register_date, administrator, blocked, image} |
| FD0102 | {username} → {id, email, password, register_date, administrator, blocked, image} |
| FD0103 | {email} → {id, username, password, register_date, administrator, blocked, image} |
| **FORMA NORMAL** | BCNF |

Tabela 4 - Validação da Relação *user*

| **TABELA R02** | notification |
| -------------- | ------------ |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD0201 | {id} → {content, date, read, id_user} |
| **FORMA NORMAL** | BCNF |

Tabela 5 - Validação da Relação *notification*

| **TABELA R03** | badge |
| -------------- | ----- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0301 | {id} → {name} |
| FD0302 | {name} → {id} |
| **FORMA NORMAL** | BCNF |

Tabela 6 - Validação da Relação *badge*

| **TABELA R04** | user_earns_badge |
| -------------- | ---------------- |
| **Chaves** | {id_user, id_badge} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 7 - Validação da Relação *user_earns_badge*

| **TABELA R05** | tag |
| -------------- | --- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0501 | {id} → {name} |
| FD0502 | {name} → {id} |
| **FORMA NORMAL** | BCNF |

Tabela 8 - Validação da Relação *tag*

| **TABELA R06** | user_follows_tag |
| -------------- | ---------------- |
| **Chaves** | {id_user, id_tag} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 9 - Validação da Relação *user_follows_tag*

| **TABELA R07** | community |
| -------------- | --------- |
| **Chaves** | {id}, {name} |
| **Dependências Funcionais** |
| FD0701 | {id} → {name} |
| FD0702 | {name} → {id} |
| **FORMA NORMAL** | BCNF |

Tabela 10 - Validação da Relação *community*

| **TABELA R08** | user_follows_community |
| -------------- | ---------------------- |
| **Chaves** | {id_user, id_community} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 11 - Validação da Relação *user_follows_community*

| **TABELA R09** | user_moderates_community |
| -------------- | ------------------------ |
| **Chaves** | {id_user, id_commmunity} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 12 - Validação da Relação *user_moderates_community*

| **TABELA R10** | reputation |
| -------------- | ---------- |
| **Chaves** | {id_user, id_community} |
| **Dependências Funcionais** |
| FD1001 | {id_user, id_community} → {rating, expert} |
| **FORMA NORMAL** | BCNF |

Tabela 13 - Validação da Relação *reputation*

| **TABELA R11** | question |
| -------------- | -------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1101 | {id} → {content, date, file, last_edited, title, id_user, id_community} |
| **FORMA NORMAL** | BCNF |

Tabela 14 - Validação da Relação *question*

| **TABELA R12** | user_follows_question |
| -------------- | --------------------- |
| **Chaves** | {id_user, id_question} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 15 - Validação da Relação *user_follows_question*

| **TABELA R13** | question_tags |
| -------------- | ------------- |
| **Chaves** | {id_question, id_tag} |
| **Dependências Funcionais** | *nenhuma* |
| **FORMA NORMAL** | BCNF |

Tabela 16 - Validação da Relação *question_tags*

| **TABELA R14** | question_vote |
| -------------- | ------------- |
| **Chaves** | {id_question, id_user} |
| **Dependências Funcionais** |
| FD1401 | {id_question, id_user} → {like} |
| **FORMA NORMAL** | BCNF |

Tabela 17 - Validação da Relação *question_vote*

| **TABELA R15** | question_comment |
| -------------- | ---------------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1501 | {id} → {content, date, last_edited, correct, id_user, id_question} |
| **FORMA NORMAL** | BCNF |

Tabela 18 - Validação da Relação *question_comment*

| **TABELA R16** | answer |
| -------------- | ------ |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1601 | {id} → {content, date, file, last_edited, correct, id_user, id_question} |
| **FORMA NORMAL** | BCNF |

Tabela 19 - Validação da Relação *answer*

| **TABELA R17** | answer_vote |
| -------------- | ----------- |
| **Chaves** | {id_answer, id_user} |
| **Dependências Funcionais** |
| FD1701 | {id_answer, id_user} → {like} |
| **FORMA NORMAL** | BCNF |

Tabela 20 - Validação da Relação *answer_vote*

| **TABELA R18** | answer_comment |
| -------------- | -------------- |
| **Chaves** | {id} |
| **Dependências Funcionais** |
| FD1801 | {id} → {content, date, last_edited, correct, id_user, id_answer} |
| **FORMA NORMAL** | BCNF |

Tabela 21 - Validação da Relação *answer_comment*


Não são necessárias quaisquer alterações para converter o esquema relacional para a Forma Normal da Boyce-Codd (BCNF), tendo em conta que todas as relações já se encontram na Forma Normal de Boyce-Codd. 

É possível comprovar este facto através da análise do conjunto de dependências funcionais associado a cada relação, dado que os atributos do lado esquerdo de cada dependência funcional de uma relação são sempre uma superchave dessa relação.
Aliás, em cada relação, o atributo iniciado por *id* é a chave primária da relação e os restantes atributos que aparecem em lados esquerdos das dependências funcionais dessa relação são chaves candidatas da relação, pois determinam funcionalmente todos os atributos da relação.


---


## A6: Índices, Gatilhos, Transações e População da Base de Dados Indexes

> Brief presentation of the artifact goals.

### 1. Carga de Trabalho da Base de Dados
 
> A study of the predicted system load (database load).
> Estimate of tuples at each relation.

| **Referência da Relação** | **Nome da Relação** | **Ordem de Magnitude** | **Crescimento Estimado** |
| ------------------------- | ------------------- | ---------------------- | ------------------------ |
| R01 | user | | |
| R02 | notification | | |
| R03 | badge | | |
| R04 | user_earns_badge | | |
| R05 | tag | | |
| R06 | user_follows_tag | | |
| R07 | community | | |
| R08 | user_follows_community | | |
| R09 | user_moderates_community | | |
| R10 | reputation | | |
| R11 | question | | |
| R12 | user_follows_question | | |
| R13 | question_tags | | |
| R14 | question_vote | | |
| R15 | question_comment | | |
| R16 | answer | | |
| R17 | answer_vote | | |
| R18 | answer_comment | | |

Tabela 22 - Carga de Trabalho da Base de Dados


### 2. índices Propostos

#### 2.1. Índices de Desempenho
 
> Indices proposed to improve performance of the identified queries.

| **Index**           | IDX01                                  |
| ---                 | ---                                    |
| **Relation**        | Relation where the index is applied    |
| **Attribute**       | Attribute where the index is applied   |
| **Type**            | B-tree, Hash, GiST or GIN              |
| **Cardinality**     | Attribute cardinality: low/medium/high |
| **Clustering**      | Clustering of the index                |
| **Justification**   | Justification for the proposed index   |
| `SQL code`                                                  ||

#### 2.2. Índices para *Full-Text Search*

> The system being developed must provide full-text search features supported by PostgreSQL. Thus, it is necessary to specify the fields where full-text search will be available and the associated setup, namely all necessary configurations, indexes definitions and other relevant details.  

| **Index**           | IDX01                                  |
| ---                 | ---                                    |
| **Relation**        | Relation where the index is applied    |
| **Attribute**       | Attribute where the index is applied   |
| **Type**            | B-tree, Hash, GiST or GIN              |
| **Clustering**      | Clustering of the index                |
| **Justification**   | Justification for the proposed index   |
| `SQL code`                                                  ||


### 3. Gatilhos
 
> User-defined functions and trigger procedures that add control structures to the SQL language or perform complex computations, are identified and described to be trusted by the database server. Every kind of function (SQL functions, Stored procedures, Trigger procedures) can take base types, composite types, or combinations of these as arguments (parameters). In addition, every kind of function can return a base type or a composite type. Functions can also be defined to return sets of base or composite values.  

| **Trigger**      | TRIGGER01                              |
| ---              | ---                                    |
| **Description**  | Trigger description, including reference to the business rules involved |
| `SQL code`                                             ||


### 4. Transações
 
> Transactions needed to assure the integrity of the data.  

| SQL Reference   | Transaction Name                    |
| --------------- | ----------------------------------- |
| Justification   | Justification for the transaction.  |
| Isolation level | Isolation level of the transaction. |
| `Complete SQL Code`                                   ||


## Anexo A. Código SQL

> The database scripts are included in this annex to the EBD component.
> 
> The database creation script and the population script should be presented as separate elements.
> The creation script includes the code necessary to build (and rebuild) the database.
> The population script includes an amount of tuples suitable for testing and with plausible values for the fields of the database.
>
> The complete code of each script must be included in the group's git repository and links added here.

### A.1. Esquema da Base de Dados

> The complete database creation must be included here and also as a script in the repository.

### A.2. População da Base de Dados

> Only a sample of the database population script may be included here, e.g. the first 10 lines. The full script must be available in the repository.


---


## Histórico de Revisões

Nada a assinalar.

***
GROUP23114, 23/10/2023

* António Henrique Martins Azevedo, up202108689@up.pt
* António Marujo Rama, up202108801@up.pt (Editor)
* Manuel Ramos Leite Carvalho Neto, up202108744@up.pt
* Matilde Isabel da Silva Simões, up202108782@up.pt