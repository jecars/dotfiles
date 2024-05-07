#### pass

Make sure the =pass= utility is installed so mbsync can get the password from the keyring.
Gmail emails should use "app passwords" instead of normal passwords.

```shell
sudo dnf install pass
gpg --gen-key
pass init mail@joshuacarrasco.com
pass insert email/mail@joshuacarrasco.com
pass insert email/info@joshuacarrasco.com
pass email/info@joshuacarrasco.com
```

#### mbsync

Make sure mbsync is installed and sync your email.
Make sure ```.mbsyncrc``` is in your home directory.

```shell
sudo dnf install isync

mkdir -p ~/.mail/info ~/.mail/mail
mbsync -V info mail
```

#### mu

Make sure mu is installed and index your emails.

```shell
sudo dnf install maildir-utils
mu init --maildir ~/.mail --my-address mail@joshuacarrasco.com info@joshuacarrasco.com
mu index
```

#### smtp auth

Create a file called ```.authinfo.gpg``` in your home directory. With the following contents.

```
machine "info" login "info@joshuacarrasco.com" password AppPasswordHere port 993
machine "smtp.gmail.com" login "info@joshuacarrasco.com" password AppPasswordHere port 587
machine "mail" login "mail@joshuacarrasco.com" password AppPasswordHere port 993
machine "smtp.gmail.com" login "mail@joshuacarrasco.com" password AppPasswordHere port 587
```
