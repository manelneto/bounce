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
| BR01 | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa publicação (*Post*) da qual é autor |
| BR02 | Se um utilizador autenticado (*User*) for apagado, o conteúdo do qual é autor mantém-se na base de dados com um autor anónimo |
| BR03 | O *rating* de um utilizador autenticado (*User*) numa comunidade (*Community*) é calculado de acordo com a fórmula 1000 x likes/(likes + dislikes) nas suas respostas (*Answer*) dentro dessa comunidade |
| BR04 | Um utilizador autenticado (*User*) é *expert* de uma comunidade (*Community*) se e só se tiver todos os emblemas (*Badge*) possíveis e um *rating* superior a 800 |
| BR05 | Os ficheiros (*file*) das publicações (*Post*) devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* |
| BR06 | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada publicação (*Post*) uma vez

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

Este artefacto engloba a carga de trabalho da base de dados, a especificação dos índices, dos gatilhos, das transações e das funções definidas pelo utilizador, bem como os *scripts* para criação da base de dados e para o seu povoamento. Deste modo, o documento contém tudo aquilo que é necessário para a implementação da base de dados.


### 1. Carga de Trabalho da Base de Dados


O esquema físico da base de dados inclui - na tabela abaixo - uma análise da carga estimada do sistema, nomeadamente através de uma previsão do número aproximado de linhas em cada tabela e do seu crescimento esperado, sendo essencial compreender a ordem de grandeza e o crescimento das relações de modo a desenvolver uma base de dados sólida e eficiente.

| **Referência da Relação** | **Nome da Relação** | **Ordem de Magnitude** | **Crescimento Estimado (por dia)** |
| ------------------------- | ------------------- | ---------------------- | ---------------------------------- |
| R01 | user | 10 k | 10 |
| R02 | notification | 100 k | 100 |
| R03 | badge | 10 | 1 |
| R04 | user_earns_badge | 10 k | 10 |
| R05 | tag | 1 k | 1 |
| R06 | user_follows_tag | 1 k | 10 |
| R07 | community | 100 | 1 |
| R08 | user_follows_community | 10 k | 10 |
| R09 | user_moderates_community | 100 | 1 |
| R10 | reputation | 10 k | 10 |
| R11 | question | 10 k | 10 |
| R12 | user_follows_question | 1 k | 10 |
| R13 | question_tags | 10 k | 10 |
| R14 | question_vote | 100 k | 100 |
| R15 | question_comment | 1 k | 10 |
| R16 | answer | 10 k | 10 |
| R17 | answer_vote | 100 k | 100 |
| R18 | answer_comment | 1 k | 10 |

Tabela 22 - Carga de Trabalho da Base de Dados


### 2. Índices Propostos

#### 2.1. Índices de Desempenho
 
Nas três tabelas abaixo propõem-se os índices a implementar de maneira a melhorar o desempenho das consultas à base de dados.

| **Índice** | IDX01 |
| ---------- | ----- |
| **Relação** | notification |
| **Atributo** | id_user |
| **Tipo** | Hash |
| **Cardinalidade** | Média |
| **Agrupamento** | Não |
| **Justificação** | A tabela *notification* é muito grande (estima-se que tenha muitas entradas) e atualizada frequentemente, pelo que a importância da existência de um índice é acrescida. Como as consultas são feitas, em princípio, para um dado utilizador e a ordenação necessária é apenas por data (dentro do mesmo utilizador), o índice mais relevante é do tipo hash. Não se propõe agrupamento da tabela devido à estimativa elevada para a sua frequência de crescimento. |
| **Código SQL** | |
```sql
CREATE INDEX user_notification ON notification USING hash (id_user);
```

Tabela 22 - Índice para a Relação *notification*

| **Índice** | IDX02 |
| ---------- | ----- |
| **Relação** | question |
| **Atributo** | id_community |
| **Tipo** | B-tree |
| **Cardinalidade** | Média |
| **Agrupamento** | Sim |
| **Justificação** | A tabela *question* será relativamente grande, mas a frequência de atualização deve ser baixa. A par disto, as operações de pesquisa e ordenação poderão ser bastante frequentes. Como tal, um índice do tipo b-tree afigura-se o mais apropriado, sobre o atributo *id_community* porque, numa porção significativa dos casos, as consultas são efetuadas a perguntas inseridas numa comunidade. Também por este motivo - e tendo em conta que não se estima que a tabela seja atualizada muito frequentemente (e a cardinalidade é média) -, propõe-se agrupamento das entradas da mesma. |
| **Código SQL** | |
```sql
CREATE INDEX community_question ON question USING btree (id_community);
CLUSTER question USING community_question;
```

Tabela 23 - Índice para a Relação *question*

| **Índice** | IDX03 |
| ---------- | ----- |
| **Relação** | question_vote |
| **Atributo** | id_question |
| **Tipo** | Hash |
| **Cardinalidade** | Média |
| **Agrupamento** | Não |
| **Justificação** | Estima-se que tabela *question_vote* tenha muitas entradas e cresça com grande frequência. Contudo, esta tabela apenas é relevante para contar o número (e tipo) de votos numa questão, bem como o utilizador que os efetuou (de maneira a impedir votos duplicados). Deste modo, as comparações efetuadas nas consultas à base de dados são apenas de igualdade e não existe necessidade de ordenação, pelo que um índice do tipo hash é o mais adequado. Para além de não funcionar com índices deste tipo, não é interessante fazer agrupamento dos dados dado que a tabela é atualizada frequentemente, com novos votos. |
| **Código SQL** | |
```sql
CREATE INDEX question_question_vote ON question_vote USING hash (id_question);
```

Tabela 24 - Índice para a Relação *question_vote*

Note-se que, dadas as semelhanças entre as relações *question* e *answer*, bem como *question_vote* e *answer_vote*, os índices a implementar para as primeiras tabelas também poderiam ser implementados para as segundas (com as devidas adaptações, mas pelos mesmos motivos).

#### 2.2. Índices para *Full-Text Search*

O sistema a desenvolver deve permitir *full-text search*, pelo que é necessário definir os índices e especificar os campos dos documentos sobre os quais eles operam. Tais definições/especificações e respetivas configurações necessárias encontram-se nas tabelas abaixo.

| **Índice** | IDX11 |
| ---------- | ----- |
| **Relação** | question |
| **Atributos** | title, content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre perguntas, incluindo o seu título e conteúdo. O tipo do índice é GIN porque, pese embora estes campos sejam editáveis, não é esperado que sejam atualizados frequentemente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela question uma coluna para armazenar os ts_vectors computados
ALTER TABLE question
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION question_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = (
            setweight(to_tsvector('english', NEW.title), 'A') ||
            setweight(to_tsvector('english', NEW.content), 'B')
        );
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF (NEW.title <> OLD.title OR NEW.content <> OLD.content) THEN
            NEW.tsvectors = (
                setweight(to_tsvector('english', NEW.title), 'A') ||
                setweight(to_tsvector('english', NEW.content), 'B')
            );
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_search_update
    BEFORE INSERT OR UPDATE ON question
    FOR EACH ROW
    EXECUTE PROCEDURE question_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_search_idx ON question USING GIN (tsvectors);
```

Tabela 26 - Índice para *Full-Text Search* sobre a Relação *question*

| **Índice** | IDX12 |
| ---------- | ----- |
| **Relação** | answer |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre respostas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora as respostas sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela answer uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION answer_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_search_update
    BEFORE INSERT OR UPDATE ON answer
    FOR EACH ROW
    EXECUTE PROCEDURE answer_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_search_idx ON answer USING GIN (tsvectors);
```

Tabela 27 - Índice para *Full-Text Search* sobre a Relação *answer*

| **Índice** | IDX13 |
| ---------- | ----- |
| **Relação** | question_comment |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre comentários em perguntas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora os comentários sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela question_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE question_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION question_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER question_comment_search_update
    BEFORE INSERT OR UPDATE ON question_comment
    FOR EACH ROW
    EXECUTE PROCEDURE question_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX question_comment_search_idx ON question_comment USING GIN (tsvectors);
```

Tabela 28 - Índice para *Full-Text Search* sobre a Relação *question_comment*

| **Índice** | IDX14 |
| ---------- | ----- |
| **Relação** | answer_comment |
| **Atributo** | content |
| **Tipo** | GIN |
| **Agrupamento** | Não |
| **Justificação** | Este índice é necessário para efetuar *full-text search* sobre comentários em respostas, nomeadamente sobre o seu conteúdo. O tipo do índice é GIN porque, pese embora os comentários sejam editáveis, não é esperado que essa edição seja frequente - é mais importante uma pesquisa rápida do que uma atualização rápida. |
| **Código SQL** | |
```sql
-- Adicionar à tabela answer_comment uma coluna para armazenar os ts_vectors computados
ALTER TABLE answer_comment
ADD COLUMN tsvectors TSVECTOR;

-- Criar uma função para atualizar automaticamente os ts_vectors
CREATE FUNCTION answer_comment_search_update() RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        NEW.tsvectors = to_tsvector('english', NEW.content);
    END IF;
    IF TG_OP = 'UPDATE' THEN
        IF NEW.content <> OLD.content THEN
            NEW.tsvectors = to_tsvector('english', NEW.content);
        END IF;
    END IF;
    RETURN NEW;
END $$
LANGUAGE plpgsql;

-- Criar um gatilho para executar antes de inserções e atualizações na tabela question
CREATE TRIGGER answer_comment_search_update
    BEFORE INSERT OR UPDATE ON answer_comment
    FOR EACH ROW
    EXECUTE PROCEDURE answer_comment_search_update();

-- Criar um índice GIN para os ts_vectors
CREATE INDEX answer_comment_search_idx ON answer_comment USING GIN (tsvectors);
```

Tabela 29 - Índice para *Full-Text Search* sobre a Relação *answer_comment*

Note-se que as funções ```answer_search_update()```, ```question_comment_search_update()``` e ```answer_comment_search_update()```, presentes nos últimos índices (IDX12, IDX13 e IDX14) - têm o corpo comum, isto é, são constituídas pelas mesmas instruções, dado que uma coluna de nome *content* está presente nas três relações (*answer*, *question_comment* e *answer_comment*, respetivamente) sobre as quais efetuar *full-text search*. Por este motivo, no *script* SQL de criação da base de dados, só se definirá a função uma vez - com o nome ```content_search_update()``` - e os gatilhos serão adaptados para chamar a função apropriada. Aqui, a função foi apresentada de forma triplicada para simplicidade na leitura/análise de cada bloco de código SQL.


### 3. Gatilhos
 
Os triggers desempenham um papel crucial na automatização de processos e na manutenção da integridade dos dados em sistemas de gestão de base de dados, garantindo a execução de ações específicas em resposta a eventos predefinidos.


| **Trigger**      | TRIGGER01                              |
| ---              | ---                                    |
| **Descrição**  | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa pergunta da qual é autor - **BR01** |
| `SQL code`       |                                        |

Tabela 30 - Gatilho para verificar *votos* numa *pergunta*

| **Trigger**      | TRIGGER02                              |
| ---              | ---                                    |
| **Descrição**  | Nenhum utilizador autenticado (*User*) pode votar (*Vote*) numa resposta da qual é autor - **BR01** |
| `SQL code`       |                                        |

Tabela 31 - Gatilho para verificar *votos* numa *resposta*

| **Trigger**      | TRIGGER03                            |
| ---              | ---                                    |
| **Descrição**  | Se um utilizador autenticado (*User*) for apagado, o conteúdo do qual é autor mantém-se na base de dados com um autor anónimo - **BR02** |
| `SQL code`       |                                        |

Tabela 32 - Gatilho para *utilizadores apagados*

| **Trigger**      | TRIGGER04                              |
| ---              | ---                                    |
| **Descrição**  | O *rating* de um utilizador autenticado (*User*) numa comunidade (*Community*) é calculado de acordo com a fórmula 1000 x likes/(likes + dislikes) nas suas respostas (*Answer*) dentro dessa comunidade - **BR03** |
| `SQL code`       |                                        |

Tabela 33 - Gatilho para calcular *rating*

| **Trigger**      | TRIGGER05                              |
| ---              | ---                                    |
| **Descrição**  | Um utilizador autenticado (*User*) é *expert* de uma comunidade (*Community*) se e só se tiver todos os emblemas (*Badge*) possíveis e um *rating* superior a 800 - **BR04** |
| `SQL code`       |                                        |

Tabela 34 - Gatilho para verificar *experts*

| **Trigger**      | TRIGGER06                              |
| ---              | ---                                    |
| **Descrição**  | Os ficheiros (*file*) das perguntas devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* - **BR05** |
| `SQL code`       |                                        |

Tabela 35 - Gatilho para verificar *extensões de ficheiros* nas *perguntas*

| **Trigger**      | TRIGGER07                              |
| ---              | ---                                    |
| **Descrição**  | Os ficheiros (*file*) das respostas devem ter uma das seguintes extensões (ou seja, terminar em) *jpg*, *jpeg*, *png*, *txt*, *pdf*, *doc* - **BR05** |
| `SQL code`       |                                        |

Tabela 36 - Gatilho para verificar *extensões de ficheiros* nas *respostas*

| **Trigger**      | TRIGGER08                              |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada pergunta uma vez - **BR06** |
| `SQL code`       |                                        |

Tabela 37 - Gatilho para verificar *votação única* nas *perguntas*

| **Trigger**      | TRIGGER09                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) só pode votar (*Vote*) em cada resposta uma vez - **BR06** |
| `SQL code`       |                                        |

Tabela 38 - Gatilho para verificar *votação única* nas *respostas*

| **Trigger**      | TRIGGER10                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) deve receber notificações de respostas às próprias perguntas |
| `SQL code`       |                                        |

Tabela 39 - Gatilho para verificar *notificação de resposta* nas *próprias perguntas*

| **Trigger**      | TRIGGER11                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) deve receber notificações de votos às próprias perguntas |
| `SQL code`       |                                        |

Tabela 40 - Gatilho para verificar *notificação de votos* nas *próprias perguntas*

| **Trigger**      | TRIGGER12                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) deve receber notificações de comentários às próprias perguntas |
| `SQL code`       |                                        |

Tabela 41 - Gatilho para verificar *notificação de comentários* nas *próprias perguntas*

| **Trigger**      | TRIGGER13                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) deve receber notificações de comentários às próprias respostas |
| `SQL code`       |                                        |

Tabela 42 - Gatilho para verificar *notificação de comentários* nas *próprias respostas*

| **Trigger**      | TRIGGER14                             |
| ---              | ---                                    |
| **Descrição**  | Cada utilizador autenticado (*User*) deve receber notificações de atribuição de emblemas |
| `SQL code`       |                                        |

Tabela 43 - Gatilho para verificar a *atribuição de emblemas*


### 4. Transações
 
As transações desempenham um papel crucial na garantia da integridade e da consistência dos dados, permitindo que múltiplos utilizadores acedam e modifiquem as mesmas informações de forma segura e coordenada.

| **Transação** | TRANXX |
| ------------- | ------ |
| **Descrição** | Associar uma *tag* a uma nova pergunta |
| **Justificação** | De maneira a manter a consistência dos dados, é necessário usar uma transação para garantir que todo o código executa sem erros. Se ocorrer um erro, faz-se ROLLBACK. O nível de isolamento é *Repeatable Read* porque, caso contrário, uma atualização de *question_id_seq* podia acontecer devido a uma inserção na tabela *question* feita por uma transação concorrente, resultando no armazenamento de dados inconsistentes. |
| **Nível de Isolamento** | REPEATABLE READ |
| **Código SQL** |  |
```sql
BEGIN TRANSACTION;

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ

-- Inserir uma pergunta
INSERT INTO question (content, date, file, last_edited, title, id_user, id_community)
VALUES ($content, $date, $file, $last_edited, $title, $id_user, $id_community);

-- Associar uma tag à pergunta
INSERT INTO question_tags (id_question, id_tag)
VALUES (currval('question_id_seq'), $id_tag);

END TRANSACTION;
```


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