FROM openjdk:11

RUN useradd --create-home --home-dir /home/MarkAbstract --shell /bin/bash operacao

USER operacao
WORKDIR /home/MarkAbstract

COPY --chown=operacao MarkAbstract-assembly-2.0.0.jar /home/MarkAbstract
COPY --chown=operacao prefixes.txt /home/MarkAbstract
COPY --chown=operacao decs/decs /home/MarkAbstract/decs


