CERT_NAME=codex.local
CRT_PATH=/etc/ssl/certs
KEY_PATH=/etc/ssl/private

ENTITY_ID=https://codex.local/shibboleth

SHIB_DEST=/etc/shibboleth/
APACHE_SITES=/etc/apache2/sites-available

.PHONY: all install certs shib-keys config enable restart

all: install certs shib-keys config enable restart

# Instala dependências e módulos necessários
install:
	sudo apt update
	sudo apt install -y libapache2-mod-shib
	sudo a2enmod shib
	sudo a2enmod ssl
	sudo a2enmod proxy
	sudo a2enmod proxy_http
	sudo systemctl restart apache2

# Gera certificado SSL para o Apache
certs:
	sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
		-keyout /etc/apache2/ssl/codex.local.key \
		-out /etc/apache2/ssl/codex.local.crt \
		-subj "/C=PT/ST=Porto/L=Porto/O=Codex/OU=Codex/CN=codex.local" 

# Gera as chaves para o Shibboleth SP
shib-keys:
	sudo shib-keygen -f -h $(CERT_NAME) -y 10 -e $(ENTITY_ID)

# Configura Apache e Shibboleth no diretorio /etc/
config:
	sudo cp codex.local.conf $(APACHE_SITES)/codex.local.conf
	sudo cp shibboleth2.xml $(SHIB_DEST)/shibboleth2.xml

# Ativa o site e adiciona ao /etc/hosts se necessário
enable:
	sudo a2ensite codex.local.conf
	@grep -q "codex.local" /etc/hosts || sudo sh -c 'echo "127.0.0.1 codex.local" >> /etc/hosts'

# Reinicia serviços relevantes
restart:
	sudo systemctl restart apache2
	sudo systemctl restart shibd
