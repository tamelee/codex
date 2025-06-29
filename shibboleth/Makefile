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
	sudo mkdir -p /etc/apache2/ssl/ && \
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
	sudo cp attribute-map.xml /etc/shibboleth/attribute-map.xml

# Ativa o site e adiciona ao /etc/hosts se necessário
enable:
	sudo a2ensite codex.local.conf
	@grep -q "codex.local" /etc/hosts || sudo sh -c 'echo "127.0.0.1 codex.local" >> /etc/hosts'

# Reinicia serviços relevantes
restart:
	sudo systemctl restart apache2
	sudo systemctl restart shibd


# -------- IMPORTANTE

# Altera a chave e certificado Shibboleth SP pela correta 
#    (presentes no diretorio atual no ficheiro zip)
# $ sudo cp sp-cert.pem /etc/shibboleth/sp-cert.pem
# $ sudo cp sp-key.pem /etc/shibboleth/sp-key.pem

# [Por vezes] pode ser necessario guardar o metadata da idp manualmente
#    (caso o Shibboleth.sso/Login nao consiga encontrar o idp da UP) 
# $ curl -o idp-up-metadata.xml https://wayf.up.pt/idp/shibboleth
# $ sudo cp idp-up-metadata.xml /etc/shibboleth/idp-up-metadata.xml

# [Por vezes] pode ser necessario atribuir permissoes 
#    (caso o shib nao consiga ler os ficheiros)
# $ sudo chmod 644 /etc/shibboleth/sp-key.pem
# $ sudo chmod 644 /etc/shibboleth/idp-up-metadata.xml

# -------------------

