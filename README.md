# HomesteadGUI

An ugly little front-end to manage your homestead instance.

![Screenshot](https://raw.githubusercontent.com/paulgeneres/HomesteadGUI/master/screenshot.png)

## Getting Started

### Prerequisities

* Windows 7+
* [VirtualBox](https://www.virtualbox.org)
* [Vagrant](https://www.vagrantup.com)
* [Laravel Homestead](https://github.com/laravel/homestead)
* [Lazarus](http://www.lazarus-ide.org)

#### Optional

* [Hosts File Editor](https://hostsfileeditor.codeplex.com/)

### Installing

* `git clone https://github.com/paulgeneres/HomesteadGUI.git`
* `cd src`
* Open `HomesteadGUI.lpr`
* Press `F9`

### Usage Notes

To launch *Hosts File Editor* from the GUI you must run with administrator permissions.

No usable YAML library bindings exist for Free-Pascal at this time. Free-Pascal does however have fairly robust support for reading and writing JSON. I take advantage of the fact that valid JSON is also valid YAML.  If `.homestead\Homestead.yaml` is in JSON format then you will be able to edit the **folders** and **sites** sections using the configuration dialog. An example `Homestead.yaml` file in JSON format is included in the repository.

## Contributing

Contributions welcome.

### To Do

* Linux support
* Mac support
* YAML file reading/writing
* Support multiple instances of *Homestead*.
* Update `hosts` based on the content of `Homestead.yaml`

## Author

* **Paul Generes** - [github](https://github.com/paulgeneres)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
